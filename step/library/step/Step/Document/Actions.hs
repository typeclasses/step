{-# language FlexibleContexts #-}

module Step.Document.Actions where

import Step.Internal.Prelude

import Step.Action
    ( nextMaybe,
      actionState,
      SureQuery,
      CursorPosition,
      IsStep(hoistStep),
      IsWalk(..),
      AtomicMove,
      castTo,
      bindAction,
      commit,
      fail,
      nextCharMaybe,
      one )
import qualified Step.Action as A

import Step.Document.Base (DocumentMemory, Context)
import qualified Step.Document.Base as Doc

import Text (Text)

import qualified Step.Nontrivial as Nontrivial
import Step.Nontrivial (Nontrivial)
import qualified Step.Nontrivial.ListLike as LL

import Step.Cursor

import Step.GeneralCursors (CursorPosition)

import Step.LineHistory (LineHistory)
import qualified Step.LineHistory as LineHistory

import Step.ContextStack (ContextStack (..), contextStackSeq)

import Step.ContextStack

import qualified Loc

contextualize :: forall act xs x s e m m' a. MonadReader (Context xs x s m) m' => IsWalk act =>
    Text
    -> act xs x e m' a
    -> act xs x e m' a
contextualize n = mapSteps (liftF . hoistStep (local f))
  where
    f :: Context xs x s m -> Context xs x s m
    f = over (Doc.ctxConfigLens % Doc.configContextLens % contextStackSeq) (n :<|)

cursorPosition :: MonadState (DocumentMemory xs x s) m => SureQuery xs x e m CursorPosition
cursorPosition = actionState <&> Doc.cursorPosition

lineHistory :: MonadState (DocumentMemory xs x s) m => SureQuery xs x e m LineHistory
lineHistory = actionState <&> Doc.lineHistory

position :: MonadState (DocumentMemory xs x s) m' => SureQuery xs x e m' Loc
position = do
    lh <- lineHistory
    cp <- cursorPosition
    case LineHistory.locateCursorInDocument cp lh of
        Nothing -> error $ "LineHistory problem: " <> show lh <> " does not contain " <> show cp
        Just (LineHistory.CursorAt x) -> return x
        Just LineHistory.CursorLocationNeedsMoreInput -> do
            _ <- nextMaybe -- Force the cursor to buffer more
            lh' <- lineHistory
            case LineHistory.locateCursorInDocument cp lh' of
                Nothing -> error $ "LineHistory problem: " <> show lh <> " does not contain " <> show cp
                Just (LineHistory.CursorAt x) -> return x
                Just LineHistory.CursorLocationNeedsMoreInput ->
                    error "LineHistory problem: after buffering more, should not need more input to determine position"

satisfyJust :: MonadReader e m => ListLike xs x => (x -> Maybe a) -> AtomicMove xs x e m a
satisfyJust = A.satisfyJust LL.leftViewOperation

nextCharMaybe :: Monad m => ListLike xs x => SureQuery xs x e m (Maybe x)
nextCharMaybe = A.nextCharMaybe LL.leftViewOperation
