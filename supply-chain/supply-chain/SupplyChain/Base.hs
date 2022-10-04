module SupplyChain.Base
  (
    {- * Kinds        -} Interface, Action,
    {- * Client       -} Client, perform, order,
    {- * Vendor       -} Vendor, vend, runVendor,
    {- * Supply       -} Supply, supplyNext, supplyProduct, (+>),
    {- * Composition  -} Connect ((>->)),
    {- * Elimination  -} run,
  )
  where

import SupplyChain.Core

import Control.Applicative
import Control.Monad
import Data.Function
import Data.Functor
import Data.Kind

vend :: Client a m (forall r. b r -> Client a m (Supply a b m r)) -> Vendor a b m
vend = Vendor

(+>) :: r -> Vendor a b m -> Supply a b m r
(+>) = flip Supply
