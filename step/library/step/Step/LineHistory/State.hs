module Step.LineHistory.State
  (
    record,
  )
  where

import Step.Internal.Prelude

import Step.LineHistory.Char (Char)
import qualified Step.LineHistory.Char as Char

import Step.LineHistory.Base (LineHistory)
import qualified Step.LineHistory.Base as LineHistory

import qualified ListLike

import qualified Map

import qualified Step.CursorPosition.Base as CursorPosition

import qualified Loc

record :: Monad m => Char char => ListLike text char => text -> StateT (LineHistory text) m ()
record x =
    case ListLike.uncons x of
        Nothing -> return ()
        Just (c, x') | c == Char.carriageReturn -> do
            recordCR
            record x'
        Just (c, x') | c == Char.lineFeed -> do
            recordLF
            record x'
        Just _ -> do
            let (a, b) = ListLike.break (`elem` [Char.carriageReturn, Char.lineFeed]) x
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

recordOther :: Monad m => ListLike text char => text -> StateT (LineHistory text) m ()
recordOther x = do
    acr <- use LineHistory.afterCRLens
    when acr startNewLine
    modifying LineHistory.cursorPositionLens (CursorPosition.increase (fromIntegral (ListLike.length x)))
    assign LineHistory.afterCRLens False
