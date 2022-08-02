{-# language FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving #-}

module Step.GeneralCursors
  (
    {- * Buffer concept and operations -} Buffer, chunks, takeChunk, dropFromBuffer,
    {- * Pure and loading cursors -} bufferStateCursor, loadingCursor,
    {- * Cursor transformers -} countingCursor, whileCursor,
  )
  where

import Step.Internal.Prelude

import Step.Nontrivial (Nontrivial, Drop (..), DropOperation (..), GeneralSpanOperation (..), Span (..))
import qualified Step.Nontrivial as Nontrivial

import Step.Cursor (Stream, AdvanceResult (..), ReadWriteCursor (ReadWriteCursor), StreamCompletion (..))
import qualified Step.Cursor as Cursor

import Step.RST (RST (..))

import Step.Input.CursorPosition (CursorPosition)
import qualified Step.Input.CursorPosition as CursorPosition

import Optics (_1, _2)

import qualified Signed
import qualified Positive
import qualified Positive.Math as Positive


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
    { Cursor.init = get <&> \s -> (view bufferLens s, s)
    , Cursor.visibleStateLens = _2
    , Cursor.input = Cursor.Stream (zoom _1 takeChunk)
    , Cursor.commit = zoom (_2 % bufferLens) . dropFromBuffer dropOp
    }

-- | Like 'bufferStateCursor', but fetches new input from a stream context when the buffer is empty
--
loadingCursor :: forall s xs x m. Monad m =>
     DropOperation xs x
     -> Lens' s (Buffer xs x) -- ^ The field of state in which the buffer is stored
     -> ReadWriteCursor xs x (Stream () s m xs x) s m
loadingCursor dropOp bufferLens =
    ReadWriteCursor{ Cursor.init, Cursor.input, Cursor.commit, Cursor.visibleStateLens }
  where
    init :: RST r s m (Buffer xs x, s)
    init = get <&> \s -> (view bufferLens s, s)

    visibleStateLens = _2

    input, bufferedInput, freshInput ::
        Stream (Stream () s m xs x) (Buffer xs x, s) m xs x
    input = Cursor.streamChoice bufferedInput freshInput
    bufferedInput = Cursor.Stream (zoom _1 takeChunk)
    freshInput = Cursor.Stream (bufferMore *> Cursor.next bufferedInput)

    commit, commitBuffered, commitFresh :: Positive Natural
        -> RST (Stream () s m xs x) (Buffer xs x, s) m AdvanceResult
    commit n = commitBuffered n >>= \case
        r@AdvanceSuccess -> return r
        YouCanNotAdvance n' -> commitFresh n'
    commitBuffered n = zoom (_2 % bufferLens) (dropFromBuffer dropOp n)
    commitFresh n = bufferMore *> commitBuffered n

    bufferMore :: RST (Stream () s m xs x) (Buffer xs x, s) m ()
    bufferMore = next >>= \case
        Nothing -> return ()
        Just x -> do
            modifying (_2 % bufferLens % chunks) (:|> x)
            modifying (_1 % chunks) (:|> x)

    next :: RST (Stream () s m xs x) (Buffer xs x, s) m (Maybe (Nontrivial xs x))
    next = ask >>= \upstream -> zoom _2 $ contravoid (Cursor.next upstream)


-- | Augments a cursor by keeping count of how many characters have been committed
--
countingCursor :: forall s xs x r m. Monad m =>
    Lens' s CursorPosition -- ^ The field of state in which the count is stored
    -> ReadWriteCursor xs x r s m
    -> ReadWriteCursor xs x r s m
countingCursor positionLens
    ReadWriteCursor{ Cursor.init = init' :: RST r s m s', Cursor.input = input', Cursor.commit = commit', Cursor.visibleStateLens = visibleStateLens' } =
    ReadWriteCursor{ Cursor.init = init', Cursor.input = input', Cursor.visibleStateLens = visibleStateLens', Cursor.commit = \n -> count n *> commit' n }
  where
    count :: Positive Natural -> RST r s' m ()
    count n = modifying (visibleStateLens' % positionLens) (CursorPosition.strictlyIncrease n)


-- | Limits a cursor to only input within a given span
--
whileCursor :: forall r s xs x m. Monad m =>
    GeneralSpanOperation xs x -> ReadWriteCursor xs x r s m -> ReadWriteCursor xs x r s m
whileCursor GeneralSpanOperation{ generalSpan }
    ReadWriteCursor{ Cursor.init = init' :: RST r s m s', Cursor.input = input', Cursor.commit = commit', Cursor.visibleStateLens = visibleStateLens' } =
    ReadWriteCursor{ Cursor.init, Cursor.input, Cursor.commit, Cursor.visibleStateLens }
  where
    init :: RST r s m (While xs x, s')
    init = (,) While{ whileCompletion = MightBeMore, whileUncommitted = 0, whileBuffer = [] } <$> init'

    visibleStateLens = _2 % visibleStateLens'

    input, bufferedInput, freshInput :: Stream r (While xs x, s') m xs x
    input = Cursor.streamChoice bufferedInput freshInput
    bufferedInput = Cursor.Stream do
        xm <- zoom (_1 % whileBufferLens) takeChunk
        traverse_ (\x -> modifying (_1 % whileUncommittedLens) (+ Nontrivial.lengthInt x)) xm
        return xm
    freshInput = Cursor.Stream $ use (_1 % whileCompletionLens) >>= \case
        Done -> return Nothing
        MightBeMore -> zoom _2 (Cursor.next input') >>= \case
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

    commit, commitBuffered, commitFresh :: Positive Natural -> RST r (While xs x, s') m AdvanceResult
    commit n = commitBuffered n >>= \case
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
                commit p
    commitFresh n = use (_1 % whileCompletionLens) >>= \case
        Done -> return (YouCanNotAdvance n)
        MightBeMore -> zoom _2 (Cursor.next input') >>= \case
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
