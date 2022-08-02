{-# language FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving #-}

module Step.GeneralCursors (Buffer, chunks, takeChunk, dropN, bufferStateCursor, loadingCursor, countingCursor) where

import Step.Internal.Prelude

import Step.Nontrivial

import qualified Step.Nontrivial as Nontrivial

import Step.Cursor (Stream, AdvanceResult (..), ReadWriteCursor (ReadWriteCursor), CursorState)
import qualified Step.Cursor as Cursor

import Step.RST (RST (..))

import Step.Input.CursorPosition (CursorPosition)
import qualified Step.Input.CursorPosition as CursorPosition


-- Buffer type

newtype Buffer xs x = Buffer{ toSeq :: Seq (Nontrivial xs x) }
    deriving newtype (Semigroup, Monoid)

instance IsList (Buffer xs x) where
    type Item (Buffer xs x) = Nontrivial xs x
    fromList = Buffer . fromList
    toList = toList . toSeq

chunks :: Iso (Buffer xs x) (Buffer xs1 x1) (Seq (Nontrivial xs x)) (Seq (Nontrivial xs1 x1))
chunks = iso toSeq Buffer


-- Buffer state operations

takeChunk :: Monad m => RST r (Buffer xs x) m (Maybe (Nontrivial xs x))
takeChunk = use chunks >>= \case
    Empty -> return Nothing
    y :<| ys -> assign chunks ys $> Just y

dropN :: Monad m => Positive Natural -> RST r (Buffer xs x) m AdvanceResult
dropN = fix \r n -> use chunks >>= \case
    Empty -> return YouCanNotAdvance{ shortfall = n }
    x :<| xs -> case drop x n of
        DroppedAll -> assign chunks xs $> AdvanceSuccess
        DroppedPart{ dropRemainder } -> assign chunks (dropRemainder :<| xs) $> AdvanceSuccess
        InsufficientToDrop{ dropShortfall } -> assign chunks xs *> r dropShortfall


-- | Cursor that just walks through a pure buffer, no streaming of additional input
--
bufferStateCursor :: forall xs x r s m. Monad m =>
    Lens' s (Buffer xs x)  -- ^ The field of state in which the buffer is stored
    -> ReadWriteCursor xs x r s m
bufferStateCursor bufferLens =
  ReadWriteCursor
    { Cursor.init = use bufferLens
    , Cursor.input = Cursor.Stream (zoom Cursor.ephemeralStateLens takeChunk)
    , Cursor.commit = zoom (Cursor.committedStateLens % bufferLens) . dropN
    }

-- | Like 'bufferStateCursor', but fetches new input from a stream context when the buffer is empty
--
loadingCursor :: forall s xs x m. Monad m =>
     Lens' s (Buffer xs x) -- ^ The field of state in which the buffer is stored
     -> ReadWriteCursor xs x (Stream () s m xs x) s m
loadingCursor bufferLens =
    ReadWriteCursor{ Cursor.init, Cursor.input, Cursor.commit }
  where
    init :: RST r s m (Buffer xs x)
    init = use bufferLens

    input, bufferedInput, freshInput ::
        Stream (Stream () s m xs x) (CursorState (Buffer xs x) s) m xs x
    input = Cursor.streamChoice bufferedInput freshInput
    bufferedInput = Cursor.Stream (zoom Cursor.ephemeralStateLens takeChunk)
    freshInput = Cursor.Stream (bufferMore *> Cursor.next bufferedInput)

    commit, commitBuffered, commitFresh :: Positive Natural
        -> RST (Stream () s m xs x) (CursorState (Buffer xs x) s) m AdvanceResult
    commit n = commitBuffered n >>= \case
        r@AdvanceSuccess -> return r
        YouCanNotAdvance n' -> commitFresh n'
    commitBuffered n = zoom (Cursor.committedStateLens % bufferLens) (dropN n)
    commitFresh n = bufferMore *> commitBuffered n

    bufferMore :: RST (Stream () s m xs x) (CursorState (Buffer xs x) s) m ()
    bufferMore = next >>= \case
        Nothing -> return ()
        Just x -> do
            modifying (Cursor.committedStateLens % bufferLens % chunks) (:|> x)
            modifying (Cursor.ephemeralStateLens % chunks) (:|> x)

    next :: RST (Stream () s m xs x) (CursorState ephemeral s) m (Maybe (Nontrivial xs x))
    next = ask >>= \upstream ->
        zoom Cursor.committedStateLens $
            contramap (\_ -> ()) (Cursor.next upstream)


-- | Augments a cursor by keeping count of how many characters have been committed
--
countingCursor :: forall s xs x r m. Monad m =>
    Lens' s CursorPosition -- ^ The field of state in which the count is stored
    -> ReadWriteCursor xs x r s m
    -> ReadWriteCursor xs x r s m
countingCursor positionLens
    ReadWriteCursor{ Cursor.init = init' :: RST r s m s', Cursor.input = input', Cursor.commit = commit' } =
    ReadWriteCursor{ Cursor.init = init', Cursor.input = input', Cursor.commit = \n -> count n *> commit' n }
  where
    count :: Positive Natural -> RST r (CursorState s' s) m ()
    count n = modifying (Cursor.committedStateLens % positionLens) (CursorPosition.strictlyIncrease n)
