module SupplyChain.Base
  (
    {- * Kinds        -} Interface, Action,
    {- * Client       -} Client, perform, request,
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

perform :: m r -> Client a m r
perform = Perform

request :: a r -> Client a m r
request = Request

vend :: Client a m (forall r. b r -> Client a m (Supply a b m r)) -> Vendor a b m
vend = Vendor

(+>) :: r -> Vendor a b m -> Supply a b m r
(+>) = flip Supply

class Connect a b m downstream result | a b m downstream -> result where
    (>->) :: Vendor a b m -> downstream -> result

instance Functor m => Connect a b m (Client b m r) (Client a m r) where
    up >-> down = connectVendorToClient up down <&> supplyProduct

instance Functor m => Connect a b m (Vendor b c m) (Vendor a c m) where
    (>->) = connectVendorToVendor
