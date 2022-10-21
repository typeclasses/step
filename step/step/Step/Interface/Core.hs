module Step.Interface.Core where

import Data.Maybe (Maybe (..))
import Data.Kind (Type)
import Numeric.Natural (Natural)
import NatOptics.Positive.Unsafe (Positive)

import SupplyChain.Interface.ResettableTerminableStream (ResettableTerminableStream)
import qualified SupplyChain.Interface.ResettableTerminableStream as RTS
import SupplyChain.Interface.Resettable (IsResettable (..))
import SupplyChain.Interface.TerminableStream (IsTerminableStream (..))

data CommittableChunkStream (chunk :: Type) (product :: Type) =
    (product ~ Maybe chunk) => NextMaybe
  | (product ~ ()) => Reset
  | (product ~ AdvanceResult) => Commit (Positive Natural)

instance IsResettable (CommittableChunkStream chunk)
  where
    reset = Reset

instance IsTerminableStream chunk (CommittableChunkStream chunk)
  where
    nextMaybe = NextMaybe

data AdvanceResult =
    AdvanceSuccess
  | YouCanNotAdvance{ shortfall :: Positive Natural }

stepCast ::
    ResettableTerminableStream chunk product
    -> CommittableChunkStream chunk product
stepCast = \case
    RTS.NextMaybe  ->  NextMaybe
    RTS.Reset      ->  Reset

commit :: Positive Natural -> CommittableChunkStream chunk AdvanceResult
commit = Commit
