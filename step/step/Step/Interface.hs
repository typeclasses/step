module Step.Interface
  (
    CommittableChunkStream, ResettableTerminableStream,
    AdvanceResult (..),
    nextMaybe, reset, commit,
    stepCast,
  )
  where

import Step.Interface.Core
import SupplyChain.Interface.ResettableTerminableStream (ResettableTerminableStream)
import SupplyChain.Interface.Resettable (IsResettable (..))
import SupplyChain.Interface.TerminableStream (IsTerminableStream (..))
