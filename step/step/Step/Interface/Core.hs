module Step.Interface.Core where

-- The basics
import Data.Maybe (Maybe (..))
import Data.Kind (Type)

-- Math
import Numeric.Natural (Natural)
import NatOptics.Positive.Unsafe (Positive)

data Mode =
    R   -- ^ Read-only
  | RW  -- ^ Read/write

data Step (mo :: Mode) (c :: Type) (a :: Type) =
    (a ~ AdvanceResult, mo ~ 'RW) => StepCommit (Positive Natural)
  | a ~ Maybe c                   => StepNext
  | a ~ ()                        => StepReset

data AdvanceResult =
    AdvanceSuccess
  | YouCanNotAdvance{ shortfall :: Positive Natural }

stepCast :: Step 'R c a -> Step 'RW c a
stepCast = \case
    StepNext  -> StepNext
    StepReset -> StepReset
