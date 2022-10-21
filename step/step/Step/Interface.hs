module Step.Interface
  (
    -- * The interface
    Mode (..), Step (..), AdvanceResult (..), stepCast,

  )
  where

import Step.Chunk
import Step.Interface.Core

-- The basics
import Data.Bool (Bool (..))
import Data.Maybe (Maybe (..), isNothing)
import Data.Functor (Functor (..), (<&>))

-- Math
import Numeric.Natural (Natural)
import NatOptics.Positive (Positive)

-- Streaming
import SupplyChain (Factory)
import qualified SupplyChain
