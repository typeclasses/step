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

import Step.ActionTypes (ContravariantAction, contramapAction)
import Step.ActionTypes.Types
import qualified Step.ActionTypes.Unsafe as Action.Unsafe

import qualified Text as T

import qualified Step.Nontrivial as Nontrivial
import Step.Nontrivial (Nontrivial)

import Step.Cursor (pattern Run, inp, com, runn)
import qualified Step.Cursor as Cursor

import Positive.Unsafe (Positive (PositiveUnsafe))

import Step.GeneralCursors (CursorPosition)

import Step.LineHistory (LineHistory)
import qualified Step.LineHistory as LineHistory

import Step.ContextStack (ContextStack (..), contextStackSeq)

takeCharMaybe :: Monad m => Sure xs x r s m (Maybe x)
takeCharMaybe = Action.Unsafe.Sure \Run{ inp, com, runn } -> runn $
    Cursor.next inp >>= \case
        Nothing -> return Nothing
        Just x -> com (PositiveUnsafe 1) $> Just (Nontrivial.head x)

peekCharMaybe :: Monad m => SureQuery xs x r s m (Maybe x)
peekCharMaybe = Action.Unsafe.SureQuery \Run{ inp, com, runn } -> runn $
    Cursor.next inp <&> \case
        Nothing -> Nothing
        Just x -> Just (Nontrivial.head x)

satisfyJust :: Monad m => (x -> Maybe a) -> AtomicMove xs x r s m a
satisfyJust ok = Action.Unsafe.AtomicMove \Run{ inp, com, runn } -> runn $
    Cursor.next inp >>= \case
        Just (ok . Nontrivial.head -> Just x) -> com (PositiveUnsafe 1) $> Right x
        _ -> ask <&> Left

atEnd :: Monad m => SureQuery xs x r s m Bool
atEnd = Action.Unsafe.SureQuery \Run{ inp, runn } -> runn $
    Cursor.next inp <&> isNothing

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
            Cursor.lookAhead_ c
            lh' <- use lineHistoryLens
            case LineHistory.locateCursorInDocument cp lh' of
                Nothing -> error $ "LineHistory problem: " <> show lh <> " does not contain " <> show cp
                Just (LineHistory.CursorAt x) -> return x
                Just LineHistory.CursorLocationNeedsMoreInput ->
                    error "LineHistory problem: after buffering more, should not need more input to determine position"

failure :: Monad m => Fail xs x r s m a
failure = Action.Unsafe.Fail \_ -> ask

some :: Monad m => AtomicMove xs x r s m (Nontrivial xs x)
some = Action.Unsafe.AtomicMove \Run{ inp, com, runn } -> runn $
    Cursor.next inp >>= \case
        Nothing -> ask <&> Left
        Just x -> com (Nontrivial.length x) $> Right x

contextualize :: Monad m => ContravariantAction act =>
    Lens' r ContextStack -> T.Text -> act xs x r s m a -> act xs x r s m a
contextualize contextStackLens n = contramapAction (over (contextStackLens % contextStackSeq) (n :<|))

-- todo: add an atomic version of 'text'

-- text :: Nontrivial xs x -> Move xs x r s m ()
-- text = someOfNontrivialText A.>=> (maybe (return ()) (cast @Any . text) . Nontrivial.refine)
--   where
--     someOfNontrivialText x = Action.Unsafe.AtomicMove $ case curse of
--         ReadWriteCursor{ init, input, commit } -> run $ Cursor.next input >>= \case
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
