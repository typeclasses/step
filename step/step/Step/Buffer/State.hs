module Step.Buffer.State where

import Chunk
import Step.Interface
import Step.Buffer.Double
import Step.Buffer.Buffer

import Data.Function (($))
import Optics (Lens', use, assign)
import Control.Monad.State.Strict (MonadState)

import SupplyChain (Vendor (..), (>->))
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
