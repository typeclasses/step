module SupplyChain.More
  (
    module SupplyChain.Base,
    Nil, nil,
  )
  where

import SupplyChain.Base

data Nil r

nil :: Nil r -> r
nil = \case{}
