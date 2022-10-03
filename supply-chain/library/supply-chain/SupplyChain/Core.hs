module SupplyChain.Core where

import Control.Applicative
import Control.Monad
import Data.Function
import Data.Functor
import Data.Kind

type I = Type -> Type

data Client (a :: I) m r
  where
    Pure    :: r   -> Client a m r
    Perform :: m r -> Client a m r
    Request :: a r -> Client a m r

    Bind :: Client a m x -> (x -> Client a m r) -> Client a m r

instance Functor m => Functor (Client a m)
  where
    fmap f = \case
        Pure    x    ->  Pure $ f x
        Perform x    ->  Perform $ fmap f x
        Request x    ->  Request x `Bind` (Pure . f)
        Bind    x g  ->  x `Bind` fmap (fmap f) g

instance Functor m => Applicative (Client a m)
  where
    pure = Pure

    a1 <*> a2 = a1 `Bind` \f -> a2 <&> \x -> f x
    a1  *> a2 = a1 `Bind` \_ -> a2

instance Functor m => Monad (Client a m) where (>>=) = Bind

newtype Vendor (a :: I) (b :: I) m =
    Vendor
      { runVendor :: Client a m (forall r. b r -> Client a m (Supply a b m r))
      }

data Supply (a :: I) (b :: I) m r =
    Supply
      { next :: Vendor a b m
      , product :: r
      }

connectVendorToClient :: Functor m => Vendor a b m -> Client b m r -> Client a m (Supply a b m r)
connectVendorToClient up = \case
    Pure    x    ->  Pure                    Supply{ next = up, product = x }
    Perform act  ->  Perform $ act <&> \x -> Supply{ next = up, product = x }
    Bind    x f  ->  connectVendorToClient up x `Bind`
                       \Supply{ next = up', product = y } ->
                          connectVendorToClient up' (f y)
    Request r    ->  connectVendorToRequest up r

connectVendorToRequest :: Vendor a b m -> b r -> Client a m (Supply a b m r)
connectVendorToRequest up r =
    case runVendor up of
        Pure h      ->                                  h r
        Perform x   ->  Perform x          `Bind` \h -> h r
        Request r'  ->  Request r'         `Bind` \h -> h r
        Bind x f    ->  x `Bind` \y -> f y `Bind` \h -> h r

connectVendorToVendor :: Functor m => Vendor a b m -> Vendor b c m -> Vendor a c m
connectVendorToVendor up (Vendor down) =
    Vendor $
        connectVendorToClient up down <&> \case
            Supply{ next = up', product = h } ->
                \r ->
                    connectVendorToClient up' (h r) <&>
                        \Supply{ next = up'', product = Supply{ next = down', product = s } } ->
                            Supply{ next = connectVendorToVendor up'' down', product = s }
