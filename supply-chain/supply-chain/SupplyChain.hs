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

    {- * Connection -}
    {- ** Functions -} vendorToJob, vendorToVendor,
    {- ** Polymorphically (>->) -} Connect (..),
    {- ** Reusing vendors -} vendorToJob',

  )
  where

import SupplyChain.Connect (Connect (..), vendorToJob, vendorToVendor, vendorToJob')

import SupplyChain.Core.Job (Job)
import SupplyChain.Core.Kinds (Type, Action, Interface)
import SupplyChain.Core.Nil (NoAction, NoInterface)
import SupplyChain.Core.Supply (Supply (..))
import SupplyChain.Core.Vendor (Vendor (..))

import qualified SupplyChain.Core.Job as Job
import qualified SupplyChain.Core.Vendor as Vendor

import Control.Monad (Monad)
import Data.Function (id)


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
