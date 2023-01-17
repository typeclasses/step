module Step.Buffer.Private where

import Block.Class
import Step.Interface
import Step.Buffer.Double
import Step.Buffer.Buffer
import Essentials

import SupplyChain (Vendor (..))
import Next.Interface (TerminableStream)

privateDoubleBuffer :: forall c up action. Block c => TerminableStream c up =>
    Vendor up (CommittableChunkStream c) action
privateDoubleBuffer = doubleBuffer (\_ -> pure ()) Empty
