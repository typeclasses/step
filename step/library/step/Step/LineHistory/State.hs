module Step.LineHistory.State
  (
    record,
  )
  where

import Step.Internal.Prelude

import Optics

import Step.LineHistory.Base (LineHistory, CursorLocation (..))
import qualified Step.LineHistory.Base as LineHistory

import qualified ListLike

import qualified Map

import qualified Step.CursorPosition.Base as CursorPosition

import qualified Loc

record :: Monad m => ListLike text Char => text -> StateT (LineHistory text) m ()
record x =
    case ListLike.uncons x of
        Nothing -> return ()
        Just ('\r', x') -> do
            recordCR
            record x'
        Just ('\n', x') -> do
            recordLF
            record x'
        Just _ -> do
            let (a, b) = ListLike.break (`elem` ['\r', '\n']) x
            recordOther a
            record b

startNewLine :: Monad m => StateT (LineHistory text) m ()
startNewLine = do
    l <- use LineHistory.lineTrackerLens
    let l' = l + 1
    cp <- use LineHistory.cursorPositionLens
    modifying LineHistory.lineStartPositionLens (Map.insert cp (fromIntegral (Loc.toNat l')))
    assign LineHistory.lineTrackerLens l'

recordCR :: Monad m => StateT (LineHistory text) m ()
recordCR = do
    acr <- use LineHistory.afterCRLens
    when acr startNewLine
    modifying LineHistory.cursorPositionLens (CursorPosition.increase 1)
    assign LineHistory.afterCRLens True

recordLF :: Monad m => StateT (LineHistory text) m ()
recordLF = do
    modifying LineHistory.cursorPositionLens (CursorPosition.increase 1)
    startNewLine
    assign LineHistory.afterCRLens False

recordOther :: Monad m => ListLike text Char => text -> StateT (LineHistory text) m ()
recordOther x = do
    acr <- use LineHistory.afterCRLens
    when acr startNewLine
    modifying LineHistory.cursorPositionLens (CursorPosition.increase (fromIntegral (ListLike.length x)))
    assign LineHistory.afterCRLens False
