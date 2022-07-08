module Step.DocumentMemory.State where

import Step.Internal.Prelude

import Loc (Loc)

import Step.DocumentMemory.Base (DocumentMemory (DocumentMemory), DocumentCursor)
import qualified Step.DocumentMemory.Base as DocumentMemory

import Step.LineHistory.Base (LineHistory)

import qualified Step.Cursor.State as Cursor.State

getPosition :: ListLike text char => Monad m => StateT (DocumentMemory text m) m Loc
getPosition =
    use (to DocumentMemory.position) >>= \case
        DocumentMemory.CursorAt x -> return x
        DocumentMemory.CursorLocationNeedsMoreInput -> do
            bufferMore
            use (to DocumentMemory.position) >>= \case
                DocumentMemory.CursorAt x -> return x
                DocumentMemory.CursorLocationNeedsMoreInput ->
                    error "DocumentMemory.State.getPosition: after buffering more, should not need more input to determine position"

bufferMore :: ListLike text char => Monad m => StateT (DocumentMemory text m) m ()
bufferMore = runCursorState Cursor.State.bufferMore

isAllBuffered :: Monad m => StateT (DocumentMemory text m) m Bool
isAllBuffered = get <&> DocumentMemory.isAllBuffered

runCursorState :: Monad m =>
    StateT (DocumentCursor text m) (StateT LineHistory m) a ->
    StateT (DocumentMemory text m) m a
runCursorState go = do
    dm <- get
    let content = DocumentMemory.content dm
    let cursor = DocumentMemory.cursor dm
    ((x, cursor'), content') <- lift $ runStateT (runStateT go cursor) content
    put DocumentMemory{ DocumentMemory.cursor = cursor', DocumentMemory.content = content' }
    return x

takeChar :: Monad m => ListLike text char => StateT (DocumentMemory text m) m (Maybe char)
takeChar = runCursorState Cursor.State.takeChar

peekCharMaybe :: Monad m => ListLike text char => StateT (DocumentMemory text m) m (Maybe char)
peekCharMaybe = runCursorState Cursor.State.peekCharMaybe

takeCharIf :: Monad m => ListLike text char => (char -> Bool) -> StateT (DocumentMemory text m) m (Maybe char)
takeCharIf ok = runCursorState (Cursor.State.takeCharIf ok)

takeCharJust :: Monad m => ListLike text char => (char -> Maybe r) -> StateT (DocumentMemory text m) m (Maybe r)
takeCharJust ok = runCursorState (Cursor.State.takeCharJust ok)

takeText :: Monad m => Eq text => Eq char => ListLike text char => text -> StateT (DocumentMemory text m) m Bool
takeText x = runCursorState (Cursor.State.takeText x)

takeAll :: Monad m => ListLike text char => StateT (DocumentMemory text m) m text
takeAll = runCursorState Cursor.State.takeAll

atEnd :: Monad m => ListLike text char => StateT (DocumentMemory text m) m Bool
atEnd = runCursorState Cursor.State.atEnd
