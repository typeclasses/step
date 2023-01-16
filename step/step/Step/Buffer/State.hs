module Step.Buffer.State where

import Block.Class
import Step.Interface
import Step.Buffer.Double
import Step.Buffer.Buffer
import Essentials

import Optics (Lens', use, assign)
import Control.Monad.State.Strict (MonadState)

import SupplyChain (Vendor (..), (>->))
import Next.Interface (TerminableStream, Next)
import qualified SupplyChain
import qualified Next as Stream

pureStepper :: forall s up action c. Block c => MonadState s action =>
    Lens' s (Buffer c) -> Vendor up (CommittableChunkStream c) action
pureStepper buffer =
    (Stream.empty :: Vendor up (Next c) action)
    >-> bufferedStepper buffer

{-| Turns an unbuffered stream (the 'IsTerminableStream' interface)
    into a buffered stream (the 'Step' interface).

    A buffer stored in the 'MonadState' context, at a position identified
    by the given 'Lens'' parameter, holds any input that has been read from
    the unbuffered stream but has not yet been committed. The remaining input,
    then, consists of anything that is in the buffer, followed by anything
    that is yet to be obtained from the unbuffered stream.
-}
bufferedStepper :: forall s up action c. Block c => MonadState s action =>
    TerminableStream c up =>
    Lens' s (Buffer c) -> Vendor up (CommittableChunkStream c) action
bufferedStepper buffer = Vendor \request -> do
    b <- SupplyChain.perform $ use buffer
    handle (doubleBuffer report b) request
  where
    report b = SupplyChain.perform (assign buffer b)
