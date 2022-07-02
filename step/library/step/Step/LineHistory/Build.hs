module Step.LineHistory.Build where

import Step.Internal.Prelude

import Step.LineHistory.Base (LineHistory)
import qualified Step.LineHistory.Base as LineHistory
import qualified Step.LineHistory.State as LineHistory.State

build :: ListLike text Char => [text] -> LineHistory text
build xs = execState (traverse_ LineHistory.State.record xs) LineHistory.empty
