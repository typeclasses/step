module SupplyChain
  (

    {- * Kinds -}
    {- ** Interface -} Interface, NoInterface,
    {- ** Action -} Action, NoAction,

    {- * Job -}
    {- ** Type -} Job,
    {- ** How to create a job -} {- $definingJobs -} order, perform,
    {- ** How to use a job -} runJob, evalJob,

    {- * Vendor -}
    {- ** Type -} Vendor (Vendor, handle), Supply (Supply),
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


{- $definingJobs

In addition to these functions for constructing jobs,
also keep in mind that 'Job' belongs to the 'Monad' class.

-}


{- $usingVendors

The most common way to use a 'Vendor' is to connect it to a 'Job' using
'vendorToJob'. However, a vendor can also be used by itself with 'runVendor'.

-}
