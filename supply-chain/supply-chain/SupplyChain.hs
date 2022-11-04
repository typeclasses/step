{- |

A "supply chain" represents a flow of information from one 'Vendor' to the next,
and so on, ultimately reaching a 'Job' that returns a product.

@
(vendor1 '>->' vendor2 '>->' vendor3 '>->' job)
@

In the above example, @vendor2@ is said to be the /client/ which is immediately
/downstream/ of @vendor1@. A job or vendor can place an 'order', which is
fulfilled by the vendor /upstream/ of it. So, the orders made by the @job@
are served by @vendor3@, the orders made by @vendor3@ are served by @vendor2@,
and so on. If @vendor1@ does not make any requests and thus does not require yet
another vendor upstream of it, then the above expression can be converted into
an action using 'runJob'.

-}

module SupplyChain
  (

    {- * Kinds -}
    {- ** Interface -} Interface, NoInterface,
    {- ** Action -} Action, NoAction,

    {- * Job -}
    {- ** Type -} Job, {- $job -}
    {- ** How to create a job -} {- $definingJobs -} order, perform,
    {- ** How to use a job -} runJob, evalJob,

    {- * Vendor -}
    {- ** Type -} Vendor (Vendor, handle), {- $vendor -} Supply (Supply),
    {- ** How to create a vendor -} {- $definingVendors -}
    {- ** How to use a vendor -} {- $usingVendors -} runVendor, evalVendor,
    {- ** Some simple vendors -} functionVendor, actionVendor, absurdVendor, map,

    {- * Connection -}
    {- ** Functions -} vendorToJob, vendorToVendor,
    {- ** Polymorphically (>->) -} Connect (..),
    {- ** Reusing vendors -} vendorToJob',

    {- * Alteration -}
    {- ** Functions -} alterJob, alterVendor,
    {- ** Polymorphically -} Alter (..),
    {- ** Of particular bits -} alterAction, alterOrder, alterAction', alterOrder',
        alterJobAction, alterJobOrder, alterVendorAction, alterVendorOrder,
        alterJobAction', alterJobOrder', alterVendorAction', alterVendorOrder',
        absurdAction, absurdOrder,

  )
  where

import SupplyChain.Core.Connect (vendorToVendor, vendorToJob, vendorToJob')
import SupplyChain.Core.Effect (Effect)
import SupplyChain.Core.Job (Job)
import SupplyChain.Core.Kinds (Type, Action, Interface)
import SupplyChain.Core.Nil (NoAction, NoInterface)
import SupplyChain.Core.Supply (Supply (..))
import SupplyChain.Core.Vendor (Vendor (..))

import qualified SupplyChain.Core.Effect as Effect
import qualified SupplyChain.Core.Job as Job
import qualified SupplyChain.Core.Vendor as Vendor

import Control.Applicative (pure)
import Control.Monad (Monad)
import Data.Function (($), id, (.))
import Data.Functor ((<&>))


alterJob :: (forall x. Effect up action x -> Job up' action' x)
    -> Job up action product -> Job up' action' product
alterJob = Job.alter

alterVendor :: (forall x. Effect up action x -> Job up' action' x)
    -> Vendor up down action -> Vendor up' down action'
alterVendor = Vendor.alter

runVendor :: Monad action => Vendor NoInterface down action
    -> down product -> action (Supply NoInterface down action product)
runVendor = Vendor.run

evalVendor :: Vendor NoInterface down NoAction
    -> down product -> Supply NoInterface down NoAction product
evalVendor = Vendor.eval

runJob :: Monad action =>
    Job NoInterface action product -> action product
runJob = Job.run

evalJob :: Job NoInterface NoAction product -> product
evalJob = Job.eval


-- | Perform an action in a job's 'Action' context

perform :: forall (up :: Interface) (action :: Action) (product :: Type).
    action product -> Job up action product

perform x = Job.Perform x id


-- | Send a request via the job's upstream 'Interface'

order :: forall (up :: Interface) (action :: Action) (response :: Type).
    up response -> Job up action response

order x = Job.Request x id


-- | A simple stateless vendor that responds to each request by applying a pure function

functionVendor :: forall (up :: Interface) (down :: Interface) (action :: Action).
    (forall response. down response -> response) -> Vendor up down action

functionVendor f = go
  where
    go = Vendor \x -> pure $ Supply (f x) go


-- | A simple stateless vendor that responds to each request by applying an effectful function

actionVendor :: forall (up :: Interface) (down :: Interface) (action :: Action).
    (forall response. down response -> action response) -> Vendor up down action

actionVendor f = go
  where
    go = Vendor \x -> perform (f x) <&> (`Supply` go)


absurdVendor :: forall (up :: Interface) (action :: Action).
    Vendor up NoInterface action

absurdVendor = Vendor \case{}


map :: forall (up :: Interface) (down :: Interface) (action :: Action).
    (forall x. down x -> up x) -> Vendor up down action
map f = go
  where
    go = Vendor \x -> order (f x) <&> (`Supply` go)


class Connect (up :: Interface) (down :: Interface)
    (action :: Action) (client :: Type) (result :: Type)
    | up client -> result
    , client -> down action
    , result -> up action
  where
    {-| Generalizes 'vendorToJob' and 'vendorToVendor'

        This operation is associative; if @a@ and @b@ are vendors and @c@ is a job,
        then @(a >-> b) >-> c@ is the same supply chain as @a >-> (b >-> c)@.
    -}
    (>->) :: Vendor up down action -> client -> result

instance Connect up down action
    (Job down action product)
    (Job up   action product)
  where
    (>->) = vendorToJob

instance Connect up middle action
    (Vendor middle down action)
    (Vendor up down action)
  where
    (>->) = vendorToVendor


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
    alter = alterJob

instance Alter up up' action action'
    (Vendor up down action)
    (Vendor up' down action')
  where
    alter = alterVendor


-- | Changes the 'Action' context

alterAction :: Alter up up action action' x1 x2 =>
    (forall x. action x -> Job up action' x) -> x1 -> x2
alterAction f = alter \case
    Effect.Perform x -> f x
    Effect.Request x -> order x

alterAction' :: Alter up up action action' x1 x2 =>
    (forall x. action x -> action' x) -> x1 -> x2
alterAction' f = alterAction (perform . f)


-- | Changes the upstream 'Interface'

alterOrder :: Alter up up' action action x1 x2 =>
    (forall x. up x -> Job up' action x) -> x1 -> x2
alterOrder f = alter \case
    Effect.Request x -> f x
    Effect.Perform x -> perform x

alterOrder' :: Alter up up' action action x1 x2 =>
    (forall x. up x -> up' x) -> x1 -> x2
alterOrder' f = alterOrder (order . f)


alterJobAction :: (forall x. action x -> Job up action' x)
    -> Job up action product -> Job up action' product
alterJobAction = alterAction

alterVendorAction :: (forall x. action x -> Job up action' x)
    -> Vendor up down action -> Vendor up down action'
alterVendorAction = alterAction

alterJobOrder :: (forall x. up x -> Job up' action x)
    -> Job up action product -> Job up' action product
alterJobOrder = alterOrder

alterVendorOrder :: (forall x. up x -> Job up' action x)
    -> Vendor up down action -> Vendor up' down action
alterVendorOrder = alterOrder


alterJobAction' :: (forall x. action x -> action' x)
    -> Job up action product -> Job up action' product
alterJobAction' = alterAction'

alterVendorAction' :: (forall x. action x -> action' x)
    -> Vendor up down action -> Vendor up down action'
alterVendorAction' = alterAction'

alterJobOrder' :: (forall x. up x -> up' x)
    -> Job up action product -> Job up' action product
alterJobOrder' = alterOrder'

alterVendorOrder' :: (forall x. up x -> up' x)
    -> Vendor up down action -> Vendor up' down action
alterVendorOrder' = alterOrder'


absurdAction :: Alter up up NoAction action' x1 x2 => x1 -> x2
absurdAction = alterAction \case{}


absurdOrder :: Alter NoInterface up' action action x1 x2 => x1 -> x2
absurdOrder = alterOrder \case{}


{- $job

>              ▲   │
>        up x  │   │  x
>              │   ▼
> ┌─────────────────────────┐
> │  Job up action product  │
> └─────────────────────────┘

-}


{- $definingJobs

In addition to these functions for constructing jobs,
also keep in mind that 'Job' belongs to the 'Monad' class.

-}


{- $vendor

>              ▲   │
>        up x  │   │  x
>              │   ▼
> ┌───────────────────────────┐
> │   Vendor up down action   │
> └───────────────────────────┘
>              ▲   │
>      down y  │   │  y
>              │   ▼

-}


{- $definingVendors

We define vendors using the v'Vendor' constructor.
Please inspect its type carefully.

> forall product. down product -> Job up action (Supply up down action product)

A vendor is a function that accepts a request. The request type is
polymorphic but constrained by the vendor's downstream interface.

> forall product. down product -> Job up action (Supply up down action product)
>                 ^^^^^^^^^^^^

A vendor has an upstream interface and can do everything a job can,
therefore the request handler operates in a 'Job' context.

> forall product. down product -> Job up action (Supply up down action product)
>                                 ^^^^^^^^^^^^^^^^^

This allows the vendor to undertake a monadic sequence involving
'order' and 'perform' while fulfilling the request.

The final step in fulfilling a request is to return a 'Supply'.

> forall product. down product -> Job up action (Supply up down action product)
>                                                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A 'Supply' is written using its '(:->)' constructor, which has two parameters:

> (:->) :: product -> Vendor up down action -> Supply up down action product

The first is the vendor's response to the client's request.

> (:->) :: product -> Vendor up down action -> Supply up down action product
>          ^^^^^^^

The second is a new 'Vendor'.

> (:->) :: product -> Vendor up down action -> Supply up down action product
>                     ^^^^^^^^^^^^^^^^^^^^^

This latter component is what allows vendors to be stateful, and it is usually
defined recursively.

-}


{- $usingVendors

The most common way to use a 'Vendor' is to connect it to a 'Job' using
'vendorToJob'. However, a vendor can also be used by itself with 'runVendor'.

-}
