{-# language FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving #-}

module Step.GeneralCursors
  (
    {- * Buffer concept and operations -} Buffer, chunks, takeChunk, dropFromBuffer,
    {- * Pure and loading cursors -} bufferStateCursor, loadingCursor,
    {- * Cursor transformers -} countingCursor, whileCursor,
  )
  where

import Step.Internal.Prelude

import Step.Nontrivial (Nontrivial, Drop (..), DropOperation (..))
import qualified Step.Nontrivial as Nontrivial

import Step.Cursor (Stream, AdvanceResult (..), ReadWriteCursor (ReadWriteCursor), CursorState, StreamCompletion (..))
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

dropFromBuffer :: Monad m =>
    DropOperation xs x
    -> Positive Natural
    -> RST r (Buffer xs x) m AdvanceResult
dropFromBuffer DropOperation{ drop } = fix \r n -> use chunks >>= \case
    Empty -> return YouCanNotAdvance{ shortfall = n }
    x :<| xs -> case drop n x of
        DropAll -> assign chunks xs $> AdvanceSuccess
        DropPart{ dropRemainder } -> assign chunks (dropRemainder :<| xs) $> AdvanceSuccess
        DropInsufficient{ dropShortfall } -> assign chunks xs *> r dropShortfall


-- | Cursor that just walks through a pure buffer, no streaming of additional input
--
bufferStateCursor :: forall xs x r s m. Monad m =>
    DropOperation xs x
    -> Lens' s (Buffer xs x)  -- ^ The field of state in which the buffer is stored
    -> ReadWriteCursor xs x r s m
bufferStateCursor dropOp bufferLens =
  ReadWriteCursor
    { Cursor.init = use bufferLens
    , Cursor.input = Cursor.Stream (zoom Cursor.ephemeralStateLens takeChunk)
    , Cursor.commit = zoom (Cursor.committedStateLens % bufferLens) . dropFromBuffer dropOp
    }

-- | Like 'bufferStateCursor', but fetches new input from a stream context when the buffer is empty
--
loadingCursor :: forall s xs x m. Monad m =>
     DropOperation xs x
     -> Lens' s (Buffer xs x) -- ^ The field of state in which the buffer is stored
     -> ReadWriteCursor xs x (Stream () s m xs x) s m
loadingCursor dropOp bufferLens =
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
    commitBuffered n = zoom (Cursor.committedStateLens % bufferLens) (dropFromBuffer dropOp n)
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


-- | Limits a cursor to only input matching the predicate
--
whileCursor :: Monad m => Predicate x -> ReadWriteCursor xs x r s m -> ReadWriteCursor xs x r s m
whileCursor ok ReadWriteCursor{ Cursor.init = init' :: RST r s m s', Cursor.input = input', Cursor.commit = commit' } =
    ReadWriteCursor{ Cursor.init, Cursor.input, Cursor.commit }
  where
    init = return While{ completion = MightBeMore, uncommitted = 0, buffer = [] }

    input :: Stream r (CursorState (While xs x) s) m xs x
    input = _

    commit :: Positive Natural -> RST r (CursorState (While xs x) s) m AdvanceResult
    commit n = _

data While xs x =
  While
    { completion :: StreamCompletion -- ^ Should we read any more from upstream
    , uncommitted :: Integer -- ^ How many characters have been sent downstream but not committed
    , buffer :: Buffer xs x -- ^ Input that has been read from upstream but not seen downstream
    }

whileCompletionLens :: Lens (While text char) (While text char) StreamCompletion StreamCompletion
whileCompletionLens = lens completion \x y -> x{ completion = y }

whileUncommittedLens :: Lens (While text char) (While text char) Integer Integer
whileUncommittedLens = lens uncommitted \x y -> x{ uncommitted = y }

whileBufferLens :: Lens (While text1 char1) (While text2 char2) (Buffer text1 char1) (Buffer text2 char2)
whileBufferLens = lens buffer \x y -> x{ buffer = y }
