module SupplyChain.Base
  (
    {- * Kinds        -} Interface, Action,
    {- * Client       -} Client, perform, order, run,
    {- * Vendor       -} Vendor, vend, runVendor,
    {- * Supply       -} Supply, supplyNext, supplyProduct, (+>),
    {- * Composition  -} Connect ((>->)),
  )
  where

import SupplyChain.Core

import Data.Function

vend :: Client a m (forall r. b r -> Client a m (Supply a b m r)) -> Vendor a b m
vend = Vendor

(+>) :: r -> Vendor a b m -> Supply a b m r
(+>) = flip Supply
