module Step.Document.Actions where

import Step.Internal.Prelude

import Step.ActionTypes

import Step.Document.Base (DocumentMemory, Context)
import qualified Step.Document.Base as Doc

import Step.CursorPosition

import Text (Text)

import qualified Step.Nontrivial as Nontrivial
import Step.Nontrivial (Nontrivial)

import Step.Cursor

import Step.GeneralCursors (CursorPosition)

import Step.LineHistory (LineHistory)
import qualified Step.LineHistory as LineHistory

import Step.ContextStack (ContextStack (..), contextStackSeq)

import Step.ContextStack

import qualified Loc

contextualize :: Monad m => ContravariantAction act => Text -> act xs x (Context xs x s m) s' e m a -> act xs x (Context xs x s m) s' e m a
contextualize n = contramapAction (over (Doc.ctxConfigLens % Doc.configContextLens % contextStackSeq) (n :<|))

cursorPosition :: Monad m => SureQuery xs x r (DocumentMemory xs x s) e m CursorPosition
cursorPosition = actionState <&> Doc.cursorPosition

lineHistory :: Monad m => SureQuery xs x r (DocumentMemory xs x s) e m LineHistory
lineHistory = actionState <&> Doc.lineHistory

position :: Monad m => SureQuery xs x (Context xs x s m) (DocumentMemory xs x s) e m Loc
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
