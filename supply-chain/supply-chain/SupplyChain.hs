module SupplyChain
  (

    {- * Job -} {- $job -} Job, order, perform, run, eval,
    {- * Vendor -} {- $vendor -} Vendor (Vendor, handle), (>->),
    {- * Referral -} {- $referral -} Referral (Referral),
    {- * Job/vendor -} {- $jobVendor -} (>-), (>+), loop, once,
    {- * Unit -} {- $unit -} Unit (Unit),

  )
  where

import SupplyChain.JobAndVendor (loop, once, (>-), (>+))
import SupplyChain.Job (Job, order, perform, run, eval)
import SupplyChain.Referral (Referral (Referral))
import SupplyChain.Vendor (Vendor (Vendor, handle), (>->))
import SupplyChain.Unit (Unit (Unit))

-- $job
-- See also: "SupplyChain.Job"

-- $vendor
-- See also: "SupplyChain.Vendor"

-- $referral
-- See also: "SupplyChain.Referral"

-- $unit
-- See also: "SupplyChain.Unit"

-- $jobVendor
-- See also: "SupplyChain.JobAndVendor"
