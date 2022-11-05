module SupplyChain.Alter
  (
    {- * Functions -} job, vendor,
    {- * Polymorphically -} Alter (..),
    {- * Of particular bits -} action, order, action', order',
        jobAction, jobOrder, vendorAction, vendorOrder,
        jobAction', jobOrder', vendorAction', vendorOrder',
        absurdAction, absurdOrder,
  )
  where

import SupplyChain.Core.Effect (Effect)
import SupplyChain.Core.Job (Job)
import SupplyChain.Core.Nil (NoAction, NoInterface)
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

class Alter up up' action action' x1 x2
    | x1 -> up action
    , x2 -> up' action'
    , x1 up' action' -> x2
    , x2 up action -> x1
  where
    -- | Generalizes 'alterJob' and 'alterVendor'
    alter :: (forall x. Effect up action x -> Job up' action' x) -> x1 -> x2

instance Alter up up' action action'
    (Job up action product) (Job up' action' product)
  where
    alter = job

instance Alter up up' action action'
    (Vendor up down action)
    (Vendor up' down action')
  where
    alter = vendor


-- | Changes the 'Action' context

action :: Alter up up action action' x1 x2 =>
    (forall x. action x -> Job up action' x) -> x1 -> x2
action f = alter \case
    Effect.Perform x -> f x
    Effect.Request x -> Job.order x

action' :: Alter up up action action' x1 x2 =>
    (forall x. action x -> action' x) -> x1 -> x2
action' f = action (Job.perform . f)


-- | Changes the upstream 'Interface'

order :: Alter up up' action action x1 x2 =>
    (forall x. up x -> Job up' action x) -> x1 -> x2
order f = alter \case
    Effect.Request x -> f x
    Effect.Perform x -> Job.perform x

order' :: Alter up up' action action x1 x2 =>
    (forall x. up x -> up' x) -> x1 -> x2
order' f = order (Job.order . f)


jobAction :: (forall x. action x -> Job up action' x)
    -> Job up action product -> Job up action' product
jobAction = action

vendorAction :: (forall x. action x -> Job up action' x)
    -> Vendor up down action -> Vendor up down action'
vendorAction = action

jobOrder :: (forall x. up x -> Job up' action x)
    -> Job up action product -> Job up' action product
jobOrder = order

vendorOrder :: (forall x. up x -> Job up' action x)
    -> Vendor up down action -> Vendor up' down action
vendorOrder = order


jobAction' :: (forall x. action x -> action' x)
    -> Job up action product -> Job up action' product
jobAction' = action'

vendorAction' :: (forall x. action x -> action' x)
    -> Vendor up down action -> Vendor up down action'
vendorAction' = action'

jobOrder' :: (forall x. up x -> up' x)
    -> Job up action product -> Job up' action product
jobOrder' = order'

vendorOrder' :: (forall x. up x -> up' x)
    -> Vendor up down action -> Vendor up' down action
vendorOrder' = order'


absurdAction :: Alter up up NoAction action' x1 x2 => x1 -> x2
absurdAction = action \case{}


absurdOrder :: Alter NoInterface up' action action x1 x2 => x1 -> x2
absurdOrder = order \case{}
