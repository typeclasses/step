module SupplyChain.Alter
  (
    {- * General -} job, vendor, request, perform,
    {- * Simplified -} job', vendor', request', perform',
  )
  where

import SupplyChain.Core.Effect (Effect)
import SupplyChain.Core.Job (Job)
import SupplyChain.Core.Vendor (Vendor (..))

import qualified SupplyChain.Core.Effect as Effect
import qualified SupplyChain.Core.Job as Job
import qualified SupplyChain.Core.Vendor as Vendor

import Data.Function ((.))

job :: (forall x. Effect up action x -> Job up' action' x)
    -> Job up action product -> Job up' action' product
job = Job.alter

vendor :: (forall x. Effect up action x -> Job up' action' x)
    -> Vendor up down action -> Vendor up' down action'
vendor = Vendor.alter

job' :: (forall x. Effect up action x -> Effect up' action' x)
    -> Job up action product -> Job up' action' product
job' f = job (Job.effect . f)

vendor' :: (forall x. Effect up action x -> Effect up' action' x)
    -> Vendor up down action -> Vendor up' down action'
vendor' f = vendor (Job.effect . f)

request :: (up product -> Effect up' action product)
    -> Effect up action product -> Effect up' action product
request = Effect.alterRequest

perform :: (action product -> Effect up action' product)
    -> Effect up action product -> Effect up action' product
perform = Effect.alterPerform

request' :: (up product -> up' product)
    -> Effect up action product -> Effect up' action product
request' f = request (Effect.Request . f)

perform' :: (action product -> action' product)
    -> Effect up action product -> Effect up action' product
perform' f = perform (Effect.Perform . f)
