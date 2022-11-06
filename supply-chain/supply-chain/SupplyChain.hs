module SupplyChain
  (

    {- * Job -} {- $job -} Job, order, perform, run, eval,
    {- * Vendor -} {- $vendor -} Vendor (Vendor, handle), Supply (Supply),
    {- * Connect -} {- $connect -} vendorToJob, vendorToVendor, Connect (..),
    {- * Interface -} Interface, NoInterface,
    {- * Action -} Action, NoAction,

  )
  where

import SupplyChain.Connect (Connect (..), vendorToJob, vendorToVendor)

import SupplyChain.Core.Job (Job, order, perform, run, eval)
import SupplyChain.Core.Kinds (Type, Action, Interface)
import SupplyChain.Core.Nil (NoAction, NoInterface)
import SupplyChain.Core.Supply (Supply (..))
import SupplyChain.Core.Vendor (Vendor (..))

-- $job
-- See also: "SupplyChain.Job"

-- $vendor
-- See also: "SupplyChain.Vendor"

-- $connect
-- See also: "SupplyChain.Connect"
