module Step.Buffer.DoubleBuffer.Optics (uncommitted, unseen) where

import Step.Internal.Prelude

import Step.Buffer.Buffer (Buffer)
import Step.Buffer.DoubleBuffer.Constructor (DoubleBuffer)
import qualified Step.Buffer.DoubleBuffer.Constructor as DB

uncommitted :: Lens' (DoubleBuffer xs x) (Buffer xs x)
uncommitted = lens DB.uncommitted \x y -> x{ DB.uncommitted = y }

unseen :: Lens' (DoubleBuffer xs x) (Buffer xs x)
unseen = lens DB.unseen \x y -> x{ DB.unseen = y }
