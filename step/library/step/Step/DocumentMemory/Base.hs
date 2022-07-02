module Step.DocumentMemory.Base where

import Step.Internal.Prelude

import Step.LineHistory.Base (LineHistory)
import qualified Step.LineHistory.Base as LineHistory
import qualified Step.LineHistory.State as LineHistory.State

import Step.Cursor.Base (Cursor)
import qualified Step.Cursor.Base as Cursor

import Loc (Loc)

data DocumentMemory text m =
  DocumentMemory
    { content :: LineHistory text
    , cursor :: Cursor (StateT (LineHistory text) m) text
    }

type DocumentCursor text m = Cursor (StateT (LineHistory text) m) text

fromListT :: ListLike text Char => Monad m => ListT m text -> DocumentMemory text m
fromListT xs =
  DocumentMemory
    { content = LineHistory.empty
    , cursor = Cursor.fromListT $ recordStream (execState . LineHistory.State.record) xs
    }

position :: DocumentMemory text m -> Loc
position x = case LineHistory.locateCursorInDocument (Cursor.position (cursor x)) (content x) of
    Just l -> l
    Nothing -> error "invalid DocumentMemory"
