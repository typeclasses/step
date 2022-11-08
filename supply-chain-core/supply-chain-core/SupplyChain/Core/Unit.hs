module SupplyChain.Core.Unit (Unit (Unit)) where

data Unit a b = a ~ b => Unit
