module Step.Buffer.Private where

import Step.Chunk
import Step.Interface
import Step.Buffer.Double
import Step.Buffer.Buffer

import Control.Applicative (Applicative (..))

import SupplyChain (Vendor (..))
import SupplyChain.Interface.TerminableStream (IsTerminableStream)

privateDoubleBuffer :: forall c up action. Chunk c => IsTerminableStream c up =>
    Vendor up (CommittableChunkStream c) action
privateDoubleBuffer = doubleBuffer (\_ -> pure ()) Empty
