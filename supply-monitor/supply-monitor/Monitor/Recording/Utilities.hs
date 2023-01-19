module Monitor.Recording.Utilities
  (
    {- * Utilities -} withRecord,
  )
  where

import Essentials
import Monitor.Interface.Type
import Monitor.Recording.Type

import SupplyChain (Job, Unit (Unit), (>-))

import qualified Monitor.Recording.Vendor as Vendor
import qualified SupplyChain.Vendor as Vendor
import qualified SupplyChain.Job as Job

withRecord :: Recording up (Unit record) state action
    -> Job up action product -> Job up action (record, product)
withRecord recording job = Vendor.recording recording >- do
    product <- Vendor.map Subject >- job
    record <- Job.order (Monitor Unit)
    pure (record, product)
