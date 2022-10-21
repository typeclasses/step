module Step.Interface
  (
    CommittableChunkStream, ResettableTerminableStream,
    AdvanceResult (..),
    ResettingSequence (..),
    nextMaybe, reset, commit,
    stepCast,
  )
  where

import Step.Interface.Core
import SupplyChain.Interface.ResettableTerminableStream (ResettableTerminableStream)
import SupplyChain.Interface.Resettable (IsResettable (..), ResettingSequence (..))
import SupplyChain.Interface.TerminableStream (IsTerminableStream (..))
