module Step.LineHistory.State where

import Step.Internal.Prelude

import Step.LineHistory.Base (LineHistory)
import qualified Step.LineHistory.Base as LineHistory

record :: Monad m => ListLike text Char => text -> StateT (LineHistory text) m ()
record x = modify' (LineHistory.record x)
