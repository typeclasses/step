module SupplyChain.Core.Unit where

data Unit a b where Unit :: a ~ b => Unit a b
