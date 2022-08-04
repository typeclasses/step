{-# language PatternSynonyms #-}
{-# language ConstraintKinds, FlexibleContexts, ViewPatterns #-}

module Step.Actions
  (
    satisfyJust, peekCharMaybe, takeCharMaybe,
    failure,
    atEnd,
    some,
    -- text,
    cursorPosition, position,
    contextualize,
  )
  where

import Step.Internal.Prelude

import Step.ActionTypes (ContravariantAction, contramapAction, LossOfMovement)
import Step.ActionTypes.Types
import qualified Step.ActionTypes.Unsafe as Action.Unsafe

import qualified Text as T

import qualified Step.Nontrivial as Nontrivial
import Step.Nontrivial (Nontrivial)

import Step.Cursor

import Positive.Unsafe (Positive (PositiveUnsafe))

import Step.GeneralCursors (CursorPosition)

import Step.LineHistory (LineHistory)
import qualified Step.LineHistory as LineHistory

import Step.ContextStack (ContextStack (..), contextStackSeq)

takeCharMaybe :: Monad m => Sure xs x r s m (Maybe x)
takeCharMaybe =
    Action.Unsafe.Sure
    \(cursorRunRW . cursorRW -> CursorRunRW{ inputRunRW, commitRunRW, runRW }) ->
    runRW $ next inputRunRW >>= \case
        Nothing -> return Nothing
        Just x -> commitRunRW (PositiveUnsafe 1) $> Just (Nontrivial.head x)

peekCharMaybe :: Monad m => SureQuery xs x r s m (Maybe x)
peekCharMaybe = Action.Unsafe.SureQuery \(cursorRunR -> CursorRunR{ inputRunR, runR }) -> runR $
    next inputRunR <&> \case
        Nothing -> Nothing
        Just x -> Just (Nontrivial.head x)

satisfyJust :: Monad m => (x -> Maybe a) -> AtomicMove xs x r s m a
satisfyJust ok = Action.Unsafe.AtomicMove \(cursorRunRW . cursorRW -> CursorRunRW{ inputRunRW, commitRunRW, runRW }) -> runRW $
    next inputRunRW >>= \case
        Just (ok . Nontrivial.head -> Just x) -> commitRunRW (PositiveUnsafe 1) $> Right x
        _ -> ask <&> Left

atEnd :: Monad m => SureQuery xs x r s m Bool
atEnd = Action.Unsafe.SureQuery \(cursorRunR -> CursorRunR{ inputRunR, runR }) ->
    runR $ next inputRunR <&> isNothing

cursorPosition :: Monad m => Lens' s CursorPosition -> SureQuery xs x r s m CursorPosition
cursorPosition o = Action.Unsafe.SureQuery \_ -> use o

position :: Monad m => Lens' s LineHistory -> Lens' s CursorPosition -> SureQuery xs x r s m Loc
position lineHistoryLens cursorPositionLens = Action.Unsafe.SureQuery \c -> do
    lh <- use lineHistoryLens
    cp <- use cursorPositionLens
    case LineHistory.locateCursorInDocument cp lh of
        Nothing -> error $ "LineHistory problem: " <> show lh <> " does not contain " <> show cp
        Just (LineHistory.CursorAt x) -> return x
        Just LineHistory.CursorLocationNeedsMoreInput -> do
            case c of
                (cursorRunR -> CursorRunR{ inputRunR, runR }) ->
                    runR (void (next inputRunR))
            lh' <- use lineHistoryLens
            case LineHistory.locateCursorInDocument cp lh' of
                Nothing -> error $ "LineHistory problem: " <> show lh <> " does not contain " <> show cp
                Just (LineHistory.CursorAt x) -> return x
                Just LineHistory.CursorLocationNeedsMoreInput ->
                    error "LineHistory problem: after buffering more, should not need more input to determine position"

failure :: Monad m => Fail xs x r s m a
failure = Action.Unsafe.Fail

some :: Monad m => AtomicMove xs x r s m (Nontrivial xs x)
some =
    Action.Unsafe.AtomicMove
    \(cursorRunRW . cursorRW -> CursorRunRW{ inputRunRW, commitRunRW, runRW }) ->
    runRW $ next inputRunRW >>= \case
        Nothing -> ask <&> Left
        Just x -> commitRunRW (Nontrivial.length x) $> Right x

contextualize :: Monad m => ContravariantAction act =>
    Lens' r ContextStack -> T.Text -> act xs x r s m a -> act xs x r s m a
contextualize contextStackLens n = contramapAction (over (contextStackLens % contextStackSeq) (n :<|))

while :: Monad m => LossOfMovement act1 act2 => Nontrivial.GeneralSpanOperation xs x
    -> act1 xs x r s m a -> act2 xs x r s m a
while = _


-- todo: add an atomic version of 'text'

-- text :: Nontrivial xs x -> Move xs x r s m ()
-- text = someOfNontrivialText A.>=> (maybe (return ()) (cast @Any . text) . Nontrivial.refine)
--   where
--     someOfNontrivialText x = Action.Unsafe.AtomicMove $ case curse of
--         CursorRW{ init, input, commit } -> run $ Cursor.next input >>= \case
--             Nothing -> return (Left F.failure)
--             Just y ->
--                 if x `Nontrivial.isPrefixOf` y
--                 then commit (Nontrivial.length x) $> Right ListLike.empty
--                 else
--                 if y `Nontrivial.isPrefixOf` x
--                 then commit (Nontrivial.length y) $>
--                       Right
--                         (
--                           ListLike.drop
--                               (ListLike.length (Nontrivial.generalize y))
--                               (Nontrivial.generalize x)
--                         )
--                 else return (Left F.failure)
