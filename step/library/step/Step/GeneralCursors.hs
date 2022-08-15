{-# language FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving #-}

module Step.GeneralCursors
  (
    {- * Pure and loading cursors -} bufferStateCursor, loadingCursor,
    {- * Cursor transformers -}
    {- ** Counting -} CursorPosition (..),
    {- ** While -} whileCursor,
  )
  where

import Step.Internal.Prelude

import Step.Nontrivial (Nontrivial, DropOperation (..), GeneralSpanOperation (..), Span (..))
import qualified Step.Nontrivial as Nontrivial

import Step.Cursor
import qualified Step.Cursor as Cursor

import Step.RST (RST (..))

import qualified Signed
import qualified Positive
import qualified Positive.Math as Positive

import Step.Buffer

import Step.Action hiding (next)


-- | Cursor that just walks through a pure buffer, no streaming of additional input
--
bufferStateCursor :: forall xs x r s m. Monad m =>
    DropOperation xs x
    -> Lens' s (Buffer xs x)  -- ^ The field of state in which the buffer is stored
    -> CursorRW xs x r s (Buffer xs x) m
bufferStateCursor dropOp bufferLens = CursorRW{ initRW, inputRW, commitRW, resetRW }
  where
    initRW = get <&> view bufferLens
    resetRW = zoom commitLens initRW >>= assign sessionLens
    inputRW = Stream (zoom sessionLens takeChunk)
    commitRW = zoom (commitLens % bufferLens) . dropFromBuffer dropOp

-- | Like 'bufferStateCursor', but fetches new input from a stream context when the buffer is empty
--
loadingCursor :: forall s xs x m. Monad m =>
     DropOperation xs x
     -> Lens' s (Buffer xs x) -- ^ The field of state in which the buffer is stored
     -> CursorRW xs x (Stream () s m xs x) s (Buffer xs x) m
loadingCursor dropOp bufferLens = CursorRW{ initRW, inputRW, commitRW, resetRW }
  where
    initRW :: RST r s m (Buffer xs x)
    initRW = get <&> view bufferLens

    resetRW = zoom commitLens initRW >>= assign sessionLens

    inputRW, bufferedInput, freshInput ::
        Stream (Stream () s m xs x) (CursorState s (Buffer xs x)) m xs x
    inputRW = streamChoice bufferedInput freshInput
    bufferedInput = Stream (zoom sessionLens takeChunk)
    freshInput = Stream (bufferMore *> next bufferedInput)

    commitRW, commitBuffered, commitFresh :: Positive Natural
        -> RST (Stream () s m xs x) (CursorState s (Buffer xs x)) m AdvanceResult
    commitRW n = commitBuffered n >>= \case
        r@AdvanceSuccess -> return r
        YouCanNotAdvance n' -> commitFresh n'
    commitBuffered n = zoom (commitLens % bufferLens) (dropFromBuffer dropOp n)
    commitFresh n = bufferMore *> commitBuffered n

    bufferMore :: RST (Stream () s m xs x) (CursorState s (Buffer xs x)) m ()
    bufferMore = next' >>= \case
        Nothing -> return ()
        Just x -> do
            modifying (commitLens % bufferLens % chunks) (:|> x)
            modifying (sessionLens % chunks) (:|> x)

    next' :: RST (Stream () s m xs x) (CursorState s (Buffer xs x)) m (Maybe (Nontrivial xs x))
    next' = ask >>= \upstream -> zoom commitLens $ contravoid (next upstream)

-- todo: While state needs to be redesigned in light of "reset"

-- | Limits a cursor to only input within a given span
--
whileCursor :: forall r s s' xs x m. Monad m =>
    GeneralSpanOperation xs x
    -> CursorRW xs x r s s' m
    -> CursorRW xs x r s (s', While xs x) m
whileCursor GeneralSpanOperation{ generalSpan } c = CursorRW{ initRW, inputRW, commitRW, resetRW }
  where
    initRW :: RST r s m (s', While xs x)
    initRW = (,)
        <$> Cursor.initRW c
        <*> pure While{ whileCompletion = MightBeMore, whileUncommitted = 0, whileBuffer = [] }

    resetRW = _

    inputRW, bufferedInput, freshInput :: Stream r (CursorState s (s', While xs x)) m xs x
    inputRW = streamChoice bufferedInput freshInput
    bufferedInput = Stream do
        xm <- zoom (sessionLens % _2 % whileBufferLens) takeChunk
        traverse_ (\x -> modifying (sessionLens % _2 % whileUncommittedLens) (+ Nontrivial.lengthInt x)) xm
        return xm
    freshInput = Stream $ use (sessionLens % _2 % whileCompletionLens) >>= \case
        Done -> return Nothing
        MightBeMore -> zoom l (next (Cursor.inputRW c)) >>= \case
            Nothing -> assign (sessionLens % _2 % whileCompletionLens) Done $> Nothing
            Just x -> case generalSpan x of
                SpanAll -> do
                    modifying (sessionLens % _2 % whileUncommittedLens) (+ Nontrivial.lengthInt x)
                    return (Just x)
                SpanNone -> do
                    assign (sessionLens % _2 % whileCompletionLens) Done
                    return Nothing
                SpanPart{ spannedPart } -> do
                    modifying (sessionLens % _2 % whileUncommittedLens) (+ Nontrivial.lengthInt spannedPart)
                    assign (sessionLens % _2 % whileCompletionLens) Done
                    return (Just spannedPart)

    l :: Lens' (CursorState s (s', While xs x)) (CursorState s s')
    l = lens
        (\(CursorState s (s', _)) -> CursorState s s')
        (\(CursorState _ (_, w)) (CursorState s s') -> CursorState s (s', w))

    commitRW, commitBuffered, commitFresh :: Positive Natural -> RST r (CursorState s (s', While xs x)) m AdvanceResult
    commitRW n = commitBuffered n >>= \case
        r@AdvanceSuccess -> return r
        YouCanNotAdvance n' -> commitFresh n'
    commitBuffered n = preuse (sessionLens % _2 % whileUncommittedLens % Positive.intPrism) >>= \case
        Nothing -> commitFresh n
        Just u -> case Positive.minus u n of
            Signed.Zero -> do
                assign (sessionLens % _2 % whileUncommittedLens) 0
                zoom l (Cursor.commitRW c n)
            Signed.Plus p -> do
                assign (sessionLens % _2 % whileUncommittedLens) (review Positive.intPrism p)
                zoom l (Cursor.commitRW c n)
            Signed.Minus p -> do
                assign (sessionLens % _2 % whileUncommittedLens) 0
                zoom l (Cursor.commitRW c u) >>= \case
                    AdvanceSuccess -> return ()
                    _ -> error "whileCursor commit"
                commitRW p
    commitFresh n = use (sessionLens % _2 % whileCompletionLens) >>= \case
        Done -> return (YouCanNotAdvance n)
        MightBeMore -> zoom l (next (Cursor.inputRW c)) >>= \case
            Nothing -> assign (sessionLens % _2 % whileCompletionLens) Done $> YouCanNotAdvance n
            Just x -> case generalSpan x of
                SpanAll -> do
                    modifying (sessionLens % _2 % whileBufferLens % chunks) (:|> x)
                    commitBuffered n
                SpanNone -> do
                    assign (sessionLens % _2 % whileCompletionLens) Done
                    return (YouCanNotAdvance n)
                SpanPart{ spannedPart } -> do
                    modifying (sessionLens % _2 % whileBufferLens % chunks) (:|> spannedPart)
                    assign (sessionLens % _2 % whileCompletionLens) Done
                    commitBuffered n

data While xs x =
  While
    { whileCompletion :: StreamCompletion -- ^ Should we read any more from upstream
    , whileUncommitted :: Integer -- ^ How many characters have been sent downstream but not committed
    , whileBuffer :: Buffer xs x -- ^ Checked input that has not been seen downstream
    }

whileCompletionLens :: Lens (While text char) (While text char) StreamCompletion StreamCompletion
whileCompletionLens = lens whileCompletion \x y -> x{ whileCompletion = y }

whileUncommittedLens :: Lens (While text char) (While text char) Integer Integer
whileUncommittedLens = lens whileUncommitted \x y -> x{ whileUncommitted = y }

whileBufferLens :: Lens (While text1 char1) (While text2 char2) (Buffer text1 char1) (Buffer text2 char2)
whileBufferLens = lens whileBuffer \x y -> x{ whileBuffer = y }
