module SupplyChain
  (

    {- * Job type -} Job,
    {- * Making jobs -} order, perform,
    {- * Running jobs -} run, eval,
    {- * Vendor type -} Vendor (Vendor, handle), Referral (Referral),
    {- * Vendor connection -} (>->),
    {- * Vendor-job connection -} (>-), (>+),
    {- * Vendor/job conversion -} loop, once, Unit (Unit),
  )
  where

import SupplyChain.JobAndVendor (loop, once, (>-), (>+))
import SupplyChain.Job (Job, order, perform, run, eval)
import SupplyChain.Referral (Referral (Referral))
import SupplyChain.Vendor (Vendor (Vendor, handle), (>->))
import SupplyChain.Unit (Unit (Unit))
