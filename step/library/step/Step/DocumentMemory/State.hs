module Step.DocumentMemory.State where

import Step.Internal.Prelude

import Loc (Loc)

import Step.DocumentMemory.Base (DocumentMemory (DocumentMemory), DocumentCursor, runCursorState)
import qualified Step.DocumentMemory.Base as DocumentMemory

import Step.LineHistory.Base (LineHistory)

import qualified Step.Cursor.State as Cursor.State

import Step.Extent.BufferedStream (Extent (Extent))

import Step.Nontrivial.Base (Nontrivial)

import Step.BufferedStream.Base (BufferedStream)

bufferMore :: ListLike text char => Monad m => StateT (DocumentMemory text m) m ()
bufferMore = runCursorState Cursor.State.bufferMore

isAllBuffered :: Monad m => StateT (DocumentMemory text m) m Bool
isAllBuffered = get <&> DocumentMemory.isAllBuffered

takeChar :: Monad m => ListLike text char => StateT (DocumentMemory text m) m (Maybe char)
takeChar = runCursorState Cursor.State.takeChar

takeCharIf :: Monad m => ListLike text char => (char -> Bool) -> StateT (DocumentMemory text m) m (Maybe char)
takeCharIf ok = runCursorState (Cursor.State.takeCharIf ok)

takeCharJust :: Monad m => ListLike text char => (char -> Maybe r) -> StateT (DocumentMemory text m) m (Maybe r)
takeCharJust ok = runCursorState (Cursor.State.takeCharJust ok)

takeTextNotAtomic :: Monad m => Eq text => Eq char => ListLike text char => text -> StateT (DocumentMemory text m) m Bool
takeTextNotAtomic x = runCursorState (Cursor.State.takeTextNotAtomic x)

takeAll :: Monad m => ListLike text char => StateT (DocumentMemory text m) m text
takeAll = runCursorState Cursor.State.takeAll

atEnd :: Monad m => ListLike text char => StateT (DocumentMemory text m) m Bool
atEnd = runCursorState Cursor.State.atEnd

-- within :: Monad m => ListLike text char =>
--     Extent (StateT (DocumentMemory text m) m) text
--     -> StateT (DocumentMemory text m) m a
--     -> StateT (DocumentMemory text m) m a
-- within (Extent
--     (e ::
--       ListT
--         (StateT
--           (BufferedStream (StateT (DocumentMemory text m) m) text)
--           (StateT (DocumentMemory text m) m)
--         )
--         (Nontrivial text)
--     )) s =
--   do
--     dm <- get
--     x <- lift $ runStateT s dm
--     _
