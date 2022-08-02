{-# language FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving #-}

module Step.GeneralCursors (Buffer, chunks, takeChunk, dropN, bufferStateCursor, loadingCursor, countingCursor) where

import Step.Internal.Prelude

import Step.Nontrivial (Nontrivial)

import qualified Step.Nontrivial as Nontrivial

import Step.Cursor (Stream, AdvanceResult (..), ReadWriteCursor (ReadWriteCursor), CursorState)
import qualified Step.Cursor as Cursor

import Step.RST (RST (..))

import Step.Input.CursorPosition (CursorPosition)
import qualified Step.Input.CursorPosition as CursorPosition

newtype Buffer xs x = Buffer{ toSeq :: Seq (Nontrivial xs x) }
    deriving newtype (Semigroup, Monoid)

instance IsList (Buffer xs x) where
    type Item (Buffer xs x) = Nontrivial xs x
    fromList = Buffer . fromList
    toList = toList . toSeq

chunks :: Iso (Buffer xs x) (Buffer xs1 x1) (Seq (Nontrivial xs x)) (Seq (Nontrivial xs1 x1))
chunks = iso toSeq Buffer

bufferStateCursor :: forall xs x r s m. (Monad m) =>
    Lens' s (Buffer xs x) -> ReadWriteCursor xs x r s m
bufferStateCursor bufferLens = ReadWriteCursor{ Cursor.init, Cursor.input, Cursor.commit }
  where
    init = use bufferLens
    input = Cursor.Stream (zoom Cursor.ephemeralStateLens takeChunk)
    commit n = zoom (Cursor.committedStateLens % bufferLens) (dropN n)

takeChunk :: Monad m => RST r (Buffer xs x) m (Maybe (Nontrivial xs x))
takeChunk = use chunks >>= \case
    Empty -> return Nothing
    y :<| ys -> assign chunks ys $> Just y

dropN :: Monad m => Positive Natural -> RST r (Buffer xs x) m AdvanceResult
dropN = fix \r n -> use chunks >>= \case
    Empty -> return YouCanNotAdvance{ shortfall = n }
    x :<| xs -> case Nontrivial.drop x n of
        Nontrivial.DroppedAll -> assign chunks xs $> AdvanceSuccess
        Nontrivial.DroppedPart{ Nontrivial.dropRemainder } -> assign chunks (dropRemainder :<| xs) $> AdvanceSuccess
        Nontrivial.InsufficientToDrop{ Nontrivial.dropShortfall } -> assign chunks xs *> r dropShortfall

countingCursor :: forall s xs x r m. Monad m =>
    Lens' s CursorPosition
    -> ReadWriteCursor xs x r s m
    -> ReadWriteCursor xs x r s m
countingCursor positionLens
    ReadWriteCursor
      { Cursor.init = init' :: RST r s m s'
      , Cursor.input = input'
      , Cursor.commit = commit'
      } =
    ReadWriteCursor{ Cursor.init, Cursor.input, Cursor.commit }
  where
    init = init'
    input = input'

    commit n = do
        modifying (Cursor.committedStateLens % positionLens) (CursorPosition.strictlyIncrease n)
        commit' n

loadingCursor :: forall s xs x m. Monad m =>
     Lens' s (Buffer xs x) -> ReadWriteCursor xs x (Stream () s m xs x) s m
loadingCursor bufferLens = ReadWriteCursor{ Cursor.init, Cursor.input, Cursor.commit }
  where
    init :: RST r s m (Buffer xs x)
    init = use bufferLens

    input, bufferedInput, freshInput :: Stream (Stream () s m xs x) (CursorState (Buffer xs x) s) m xs x
    input = Cursor.streamChoice bufferedInput freshInput
    bufferedInput = Cursor.Stream (zoom Cursor.ephemeralStateLens takeChunk)
    freshInput = Cursor.Stream (bufferMore *> Cursor.next bufferedInput)

    commit, commitBuffered, commitFresh :: Positive Natural -> RST (Stream () s m xs x) (CursorState (Buffer xs x) s) m AdvanceResult
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
