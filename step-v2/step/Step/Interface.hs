module Step.Interface
  (
    CommittableChunkStream, ResettableTerminableStream,
    AdvanceResult (..),
    ResettingSequence (..),
    next, Step (..), reset, commit,
    stepCast,
  )
  where

import Step.Interface.Core
import SupplyChain.Interface.ResettableTerminableStream (ResettableTerminableStream)
import SupplyChain.Interface.Resettable (IsResettable (..), ResettingSequence (..))
import Next.Interface (next, Step (..))
