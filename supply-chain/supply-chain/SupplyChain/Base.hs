module SupplyChain.Base
  (
    {- * Kinds        -} Interface, Action,
    {- * Client       -} Client, perform, order, run, eval,
    {- * Vendor       -} Vendor, vend, runVendor,
    {- * Supply       -} Supply, supplyNext, supplyProduct, (+>),
    {- * Composition  -} Connect ((>->)),
  )
  where

import SupplyChain.Core

import Control.Applicative (pure)
import Data.Function ((.), flip)
import Data.Functor.Identity (Identity (..))

vend :: Client a m (forall r. b r -> Client a m (Supply a b m r)) -> Vendor a b m
vend = Vendor

(+>) :: r -> Vendor a b m -> Supply a b m r
(+>) = flip Supply

eval :: (forall x. up x -> x) -> Client up Identity product -> product
eval f = runIdentity . run (pure . f)
