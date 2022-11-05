module SupplyChain
  (

    {- * Job -} {- $job -} Job, order, {-$ jobConstruction -} perform, run, eval,
    {- * Vendor -} {- $vendor -} Vendor (Vendor, handle), Supply (Supply),
    {- * Connection -} {- $ connect -} vendorToJob, vendorToVendor, Connect (..),
    {- * Interface -} Interface, NoInterface,
    {- * Action -} Action, NoAction,

  )
  where

import SupplyChain.Connect (Connect (..), vendorToJob, vendorToVendor)

import SupplyChain.Core.Job (Job)
import SupplyChain.Core.Kinds (Type, Action, Interface)
import SupplyChain.Core.Nil (NoAction, NoInterface)
import SupplyChain.Core.Supply (Supply (..))
import SupplyChain.Core.Vendor (Vendor (..))

import qualified SupplyChain.Core.Job as Job

import Control.Monad (Monad)
import Data.Function (id)

-- | Perform an action in a job's 'Action' context
perform :: forall (up :: Interface) (action :: Action) (product :: Type).
    action product -> Job up action product
perform x = Job.Perform x id

-- | Send a request via the job's upstream 'Interface'
order :: forall (up :: Interface) (action :: Action) (response :: Type).
    up response -> Job up action response
order x = Job.Request x id

-- | Run a job in its 'Action' context
run :: forall (action :: Action) (product :: Type). Monad action =>
    Job NoInterface action product -> action product
run = Job.run

-- | Run a job that performs no actions
eval :: forall (product :: Type). Job NoInterface NoAction product -> product
eval = Job.eval

{- $job

See also: "SupplyChain.Job"

-}

{- $jobConstruction

In addition to these functions for constructing jobs, also keep in mind that
'Job' belongs to the 'Monad' class.

-}

{- $vendor

The most common way to use a 'Vendor' is to connect it to a 'Job' using
'vendorToJob'.

See also: "SupplyChain.Vendor"

-}

{- $connect

See also: "SupplyChain.Connect"

-}
