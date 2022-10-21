module SupplyChain.Interface.Resettable where

import SupplyChain

class IsResettable (i :: Interface)
  where
    reset :: i ()
