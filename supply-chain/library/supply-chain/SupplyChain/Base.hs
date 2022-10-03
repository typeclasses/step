module SupplyChain.Base
  (
    {- * Types        -} Client, Vendor, Supply (..),
    {- * Introduction -} vend, perform, request, (+>),
    {- * Elimination  -} run,
    {- * Composition  -} Connect ((>->)),
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

run :: forall a m r. Monad m => (forall x. a x -> m x) -> Client a m r -> m r
run z = go
  where
    go :: forall r'. Monad m => Client a m r' -> m r'
    go = \case
      Pure    x    ->  pure x
      Perform x    ->  x
      Bind    x f  ->  go x >>= (go . f)
      Request x    ->  z x

class Connect a b m downstream result | a b m downstream -> result where
    (>->) :: Vendor a b m -> downstream -> result

instance Functor m => Connect a b m (Client b m r) (Client a m r) where
    up >-> down = connectVendorToClient up down <&> product

instance Functor m => Connect a b m (Vendor b c m) (Vendor a c m) where
    (>->) = connectVendorToVendor
