module Step.Buffer.DoubleBuffer.Extra (newDoubleBuffer) where

import Step.Buffer.Buffer (Buffer)
import Step.Buffer.DoubleBuffer.Constructor (DoubleBuffer)
import qualified Step.Buffer.DoubleBuffer.Constructor as DB

newDoubleBuffer :: Buffer xs x -> DoubleBuffer xs x
newDoubleBuffer x = DB.DoubleBuffer{ DB.unseen = x, DB.uncommitted = x }
