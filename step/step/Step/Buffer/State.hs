module Step.Buffer.State where

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

pureStepper :: forall s up action c. Chunk c => MonadState s action =>
    Lens' s (Buffer c) -> Vendor up (CommittableChunkStream c) action
pureStepper buffer =
    (Stream.nil :: Vendor up (TerminableStream c) action)
    >-> bufferedStepper buffer

{-| Turns an unbuffered stream (the 'IsTerminableStream' interface)
    into a buffered stream (the 'Step' interface).

    A buffer stored in the 'MonadState' context, at a position identified
    by the given 'Lens'' parameter, holds any input that has been read from
    the unbuffered stream but has not yet been committed. The remaining input,
    then, consists of anything that is in the buffer, followed by anything
    that is yet to be obtained from the unbuffered stream.
-}
bufferedStepper :: forall s up action c. Chunk c => MonadState s action =>
    IsTerminableStream c up =>
    Lens' s (Buffer c) -> Vendor up (CommittableChunkStream c) action
bufferedStepper buffer = Vendor \request -> do
    b <- SupplyChain.perform $ use buffer
    handle (doubleBuffer report b) request
  where
    report b = SupplyChain.perform (assign buffer b)
