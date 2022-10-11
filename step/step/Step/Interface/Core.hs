module Step.Interface.Core where

-- The basics
import Data.Maybe (Maybe (..))
import Data.Functor (Functor (..), (<$>))
import Data.Function (($), (.), id)
import Data.Either (Either (..))
import Control.Monad (Monad (..))
import qualified Control.Monad as Monad
import Control.Applicative (Applicative (..))
import Data.Kind (Type)
import Prelude (error)

-- Math
import Numeric.Natural (Natural)
import NatOptics.Positive.Unsafe (Positive)

-- Transformers
import qualified Control.Monad.Reader as MTL
import Control.Monad.Trans.Except (ExceptT (..))
import qualified Control.Monad.Trans.Except as MTL

-- Streaming
import SupplyChain (Factory, (>->), order)
import qualified SupplyChain

-- Etc
import GHC.TypeLits (TypeError, ErrorMessage (Text))


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
