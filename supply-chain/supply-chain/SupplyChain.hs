module SupplyChain
  (

    {- * Job -} {- $job -} Job, order, perform, run, eval,
    {- * Vendor -} {- $vendor -} Vendor (Vendor, handle), Supply (Supply),
    {- * Connect -} {- $connect -} vendorToJob, vendorToVendor, Connect ((>->)),

  )
  where

import SupplyChain.Connect (Connect ((>->)), vendorToJob, vendorToVendor)
import SupplyChain.Job (Job, order, perform, run, eval)
import SupplyChain.Supply (Supply (Supply))
import SupplyChain.Vendor (Vendor (Vendor, handle))

-- $job
-- See also: "SupplyChain.Job"

-- $vendor
-- See also: "SupplyChain.Vendor"

-- $connect
-- See also: "SupplyChain.Connect"
