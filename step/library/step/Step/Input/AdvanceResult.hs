module Step.Input.AdvanceResult where

import Step.Internal.Prelude

data AdvanceResult = Success | InsufficientInput{ shortfall :: Positive Natural }
