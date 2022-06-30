module Step.DocumentMemory.State where

import Step.Internal.Prelude

import Loc (Loc)

import Step.DocumentMemory.Base (DocumentMemory (DocumentMemory), DocumentCursor)
import qualified Step.DocumentMemory.Base as DocumentMemory

import Step.LineHistory.Base (LineHistory)

import qualified Step.Cursor.State as Cursor.State

getPosition :: Monad m => StateT (DocumentMemory text m) m Loc
getPosition = get <&> DocumentMemory.position

runCursorState :: Monad m =>
    StateT (DocumentCursor text m) (StateT (LineHistory text) m) a ->
    StateT (DocumentMemory text m) m a
runCursorState go = do
    dm <- get
    let content = DocumentMemory.content dm
    let cursor = DocumentMemory.cursor dm
    ((x, cursor'), content') <- lift $ runStateT (runStateT go cursor) content
    put DocumentMemory{ DocumentMemory.cursor = cursor', DocumentMemory.content = content' }
    return x

takeChar :: Monad m => ListLike text Char => StateT (DocumentMemory text m) m (Maybe Char)
takeChar = runCursorState Cursor.State.takeChar

takeCharIf :: Monad m => ListLike text Char => (Char -> Bool) -> StateT (DocumentMemory text m) m (Maybe Char)
takeCharIf ok = runCursorState (Cursor.State.takeCharIf ok)

takeText :: Monad m => Eq text => ListLike text Char => text -> StateT (DocumentMemory text m) m Bool
takeText x = runCursorState (Cursor.State.takeText x)

takeAll :: Monad m => ListLike text Char => StateT (DocumentMemory text m) m text
takeAll = runCursorState Cursor.State.takeAll
