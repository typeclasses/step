module SupplyChain.Core.Effect where

import Data.Kind (Type)
import SupplyChain.Core.Kinds (Action, Interface)

data T (up :: Interface) (action :: Action) (product :: Type) =
    Request (up product)
  | Perform (action product)
