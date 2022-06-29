module Step.DocumentMemory.Base where

import Step.Internal.Prelude

import Step.LineHistory.Base (LineHistory)
import qualified Step.LineHistory.Base as LineHistory

import Step.CountingBufferedStream.Base (CountingBufferedStream)
import qualified Step.CountingBufferedStream.Base as CountingBufferedStream

data DocumentMemory text m =
  DocumentMemory
    { lineHistory :: LineHistory text
    , pending :: CountingBufferedStream m text
    }

fromListT :: ListT m text -> DocumentMemory text m
fromListT x =
  DocumentMemory
    { lineHistory = LineHistory.empty
    , pending = CountingBufferedStream.fromListT x
    }
