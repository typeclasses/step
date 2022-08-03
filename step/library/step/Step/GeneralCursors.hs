{-# language FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving #-}

module Step.GeneralCursors
  (
    {- * Pure and loading cursors -} bufferStateCursor, loadingCursor,
    {- * Cursor transformers -}
    {- ** Counting -} countingCursor, CursorPosition (..),
    {- ** While -} whileCursor,
  )
  where

import Step.Internal.Prelude

import Step.Nontrivial (Nontrivial, DropOperation (..), GeneralSpanOperation (..), Span (..))
import qualified Step.Nontrivial as Nontrivial

import Step.Cursor
import qualified Step.Cursor as Cursor

import Step.RST (RST (..))

import Optics (_1, _2)

import Step.CursorPosition (CursorPosition)
import qualified Step.CursorPosition as CursorPosition

import qualified Signed
import qualified Positive
import qualified Positive.Math as Positive

import Step.Buffer


-- | Cursor that just walks through a pure buffer, no streaming of additional input
--
bufferStateCursor :: forall xs x r s m. Monad m =>
    DropOperation xs x
    -> Lens' s (Buffer xs x)  -- ^ The field of state in which the buffer is stored
    -> Cursor xs x r s m
bufferStateCursor dropOp bufferLens = Cursor{ cursorRW, cursorR }
  where
    cursorRW = CursorRW
      { initRW = get <&> \s -> (view bufferLens s, s)
      , visibleStateLensRW = _2
      , inputRW = Stream (zoom _1 takeChunk)
      , commitRW = zoom (_2 % bufferLens) . dropFromBuffer dropOp
      }
    cursorR = _

-- | Like 'bufferStateCursor', but fetches new input from a stream context when the buffer is empty
--
loadingCursor :: forall s xs x m. Monad m =>
     DropOperation xs x
     -> Lens' s (Buffer xs x) -- ^ The field of state in which the buffer is stored
     -> Cursor xs x (Stream () s m xs x) s m
loadingCursor dropOp bufferLens = Cursor{ cursorRW, cursorR }
  where
    cursorR = _

    cursorRW = CursorRW{ initRW, inputRW, commitRW, visibleStateLensRW }
      where
        initRW :: RST r s m (Buffer xs x, s)
        initRW = get <&> \s -> (view bufferLens s, s)

        visibleStateLensRW = _2

        inputRW, bufferedInput, freshInput ::
            Stream (Stream () s m xs x) (Buffer xs x, s) m xs x
        inputRW = streamChoice bufferedInput freshInput
        bufferedInput = Stream (zoom _1 takeChunk)
        freshInput = Stream (bufferMore *> next bufferedInput)

        commitRW, commitBuffered, commitFresh :: Positive Natural
            -> RST (Stream () s m xs x) (Buffer xs x, s) m AdvanceResult
        commitRW n = commitBuffered n >>= \case
            r@AdvanceSuccess -> return r
            YouCanNotAdvance n' -> commitFresh n'
        commitBuffered n = zoom (_2 % bufferLens) (dropFromBuffer dropOp n)
        commitFresh n = bufferMore *> commitBuffered n

        bufferMore :: RST (Stream () s m xs x) (Buffer xs x, s) m ()
        bufferMore = next' >>= \case
            Nothing -> return ()
            Just x -> do
                modifying (_2 % bufferLens % chunks) (:|> x)
                modifying (_1 % chunks) (:|> x)

        next' :: RST (Stream () s m xs x) (Buffer xs x, s) m (Maybe (Nontrivial xs x))
        next' = ask >>= \upstream -> zoom _2 $ contravoid (next upstream)


-- | Augments a cursor by keeping count of how many characters have been committed
--
countingCursor :: forall s xs x r m. Monad m =>
    Lens' s CursorPosition -- ^ The field of state in which the count is stored
    -> Cursor xs x r s m
    -> Cursor xs x r s m
countingCursor positionLens c = Cursor{ cursorRW, cursorR }
  where
    cursorR = _
    cursorRW = case Cursor.cursorRW c of
      CursorRW
        { initRW = init' :: RST r s m s'
        , inputRW = input'
        , commitRW = commit'
        , visibleStateLensRW = visibleStateLens'
        } ->
          CursorRW
            { initRW = init'
            , inputRW = input'
            , visibleStateLensRW = visibleStateLens'
            , commitRW = \n -> count n *> commit' n
            }
        where
          count :: Positive Natural -> RST r s' m ()
          count n = modifying (visibleStateLens' % positionLens) $
              appEndo $ CursorPosition.strictlyIncrease n

-- | Limits a cursor to only input within a given span
--
whileCursor :: forall r s xs x m. Monad m =>
    GeneralSpanOperation xs x
    -> Cursor xs x r s m
    -> Cursor xs x r s m
whileCursor GeneralSpanOperation{ generalSpan } c = Cursor{ cursorR, cursorRW }
  where
    cursorR = _
    cursorRW = case Cursor.cursorRW c of
      CursorRW
        { initRW = init' :: RST r s m s'
        , inputRW = input'
        , commitRW = commit'
        , visibleStateLensRW = visibleStateLens'
        } ->
          CursorRW
            { initRW
            , inputRW
            , commitRW
            , visibleStateLensRW
            }
        where
          initRW :: RST r s m (While xs x, s')
          initRW = (,) While{ whileCompletion = MightBeMore, whileUncommitted = 0, whileBuffer = [] } <$> init'

          visibleStateLensRW = _2 % visibleStateLens'

          inputRW, bufferedInput, freshInput :: Stream r (While xs x, s') m xs x
          inputRW = streamChoice bufferedInput freshInput
          bufferedInput = Stream do
              xm <- zoom (_1 % whileBufferLens) takeChunk
              traverse_ (\x -> modifying (_1 % whileUncommittedLens) (+ Nontrivial.lengthInt x)) xm
              return xm
          freshInput = Stream $ use (_1 % whileCompletionLens) >>= \case
              Done -> return Nothing
              MightBeMore -> zoom _2 (next input') >>= \case
                  Nothing -> assign (_1 % whileCompletionLens) Done $> Nothing
                  Just x -> case generalSpan x of
                      SpanAll -> do
                          modifying (_1 % whileUncommittedLens) (+ Nontrivial.lengthInt x)
                          return (Just x)
                      SpanNone -> do
                          assign (_1 % whileCompletionLens) Done
                          return Nothing
                      SpanPart{ spannedPart } -> do
                          modifying (_1 % whileUncommittedLens) (+ Nontrivial.lengthInt spannedPart)
                          assign (_1 % whileCompletionLens) Done
                          return (Just spannedPart)

          commitRW, commitBuffered, commitFresh :: Positive Natural -> RST r (While xs x, s') m AdvanceResult
          commitRW n = commitBuffered n >>= \case
              r@AdvanceSuccess -> return r
              YouCanNotAdvance n' -> commitFresh n'
          commitBuffered n = preuse (_1 % whileUncommittedLens % Positive.intPrism) >>= \case
              Nothing -> commitFresh n
              Just u -> case Positive.minus u n of
                  Signed.Zero -> do
                      assign (_1 % whileUncommittedLens) 0
                      zoom _2 (commit' n)
                  Signed.Plus p -> do
                      assign (_1 % whileUncommittedLens) (review Positive.intPrism p)
                      zoom _2 (commit' n)
                  Signed.Minus p -> do
                      assign (_1 % whileUncommittedLens) 0
                      zoom _2 (commit' u) >>= \case AdvanceSuccess -> return (); _ -> error "whileCursor commit"
                      commitRW p
          commitFresh n = use (_1 % whileCompletionLens) >>= \case
              Done -> return (YouCanNotAdvance n)
              MightBeMore -> zoom _2 (next input') >>= \case
                  Nothing -> assign (_1 % whileCompletionLens) Done $> YouCanNotAdvance n
                  Just x -> case generalSpan x of
                      SpanAll -> do
                          modifying (_1 % whileBufferLens % chunks) (:|> x)
                          commitBuffered n
                      SpanNone -> do
                          assign (_1 % whileCompletionLens) Done
                          return (YouCanNotAdvance n)
                      SpanPart{ spannedPart } -> do
                          modifying (_1 % whileBufferLens % chunks) (:|> spannedPart)
                          assign (_1 % whileCompletionLens) Done
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
