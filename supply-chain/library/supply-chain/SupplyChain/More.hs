module SupplyChain.More
  (
    module SupplyChain.Base,
    Nil, nil,
    Either' (..), bivend,
  )
  where

import SupplyChain.Base

import Control.Applicative
import Data.Function
import Data.Functor

data Nil r

nil :: Nil r -> r
nil = \case{}

data Either' a b r = Left' (a r) | Right' (b r)

bivend :: Functor m => Vendor u a m -> Vendor u b m -> Vendor u (Either' a b) m
bivend a b = vend $ pure \case
    Left' r -> _
    Right' r -> _
