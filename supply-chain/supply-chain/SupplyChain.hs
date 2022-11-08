module SupplyChain
  (

    {- * Job -} {- $job -} Job, order, perform, run, eval,
    {- * Vendor -} {- $vendor -} Vendor (Vendor, handle),
    {- * Referral -} {- $referral -} Referral (Referral),
    {- * Connect -} {- $connect -} (>->), vendorToJob', loop, once,

  )
  where

import SupplyChain.Connect ((>->), vendorToJob', loop, once)
import SupplyChain.Job (Job, order, perform, run, eval)
import SupplyChain.Referral (Referral (Referral))
import SupplyChain.Vendor (Vendor (Vendor, handle))

-- $job
-- See also: "SupplyChain.Job"

-- $vendor
-- See also: "SupplyChain.Vendor"

-- $referral
-- See also: "SupplyChain.Referral"

-- $connect
-- See also: "SupplyChain.Connect"
