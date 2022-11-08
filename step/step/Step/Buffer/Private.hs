module Step.Buffer.Private where

import Step.Chunk
import Step.Interface
import qualified Step.Interface.Core as I
import Step.Buffer.Double
import Step.Buffer.Buffer

-- The basics
import Data.Bool (Bool (..))
import Data.Maybe (Maybe (..), isNothing)
import Data.Functor (Functor (..), (<&>), ($>), (<$>))
import Data.Function (($))
import Control.Monad (Monad (..))
import Control.Applicative (Applicative (..))

-- Containers
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

-- Optics
import Optics (Lens', use, assign, modifying)

-- Math
import Numeric.Natural (Natural)
import NatOptics.Positive.Unsafe (Positive)

-- Transformers
import Control.Monad.State.Strict (MonadState)

-- Streaming
import SupplyChain (Vendor (..), Job, Referral (..), (>->), order)
import SupplyChain.Interface.TerminableStream (IsTerminableStream, TerminableStream)
import qualified SupplyChain
import qualified SupplyChain.Interface.TerminableStream as Stream

privateDoubleBuffer :: forall c up action. Chunk c => IsTerminableStream c up =>
    Vendor up (CommittableChunkStream c) action
privateDoubleBuffer = doubleBuffer (\_ -> pure ()) Empty
