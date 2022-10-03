module SupplyChain.Core where

import Control.Applicative (Applicative (pure, (*>), (<*>)), (<$>))
import Control.Monad (Monad((>>=)), Functor (fmap))
import Data.Function (($), (.))
import Data.Functor (Functor (fmap), (<$>), (<&>))
import Data.Kind (Type)

type Action = Type -> Type

type Interface = Type -> Type

data Client (up :: Interface) (action :: Action) (product :: Type)
  where
    Pure    ::        product -> Client up action product
    Perform :: action product -> Client up action product
    Request :: up     product -> Client up action product

    Bind :: Client up action x
         -> (x -> Client up action product)
         ->       Client up action product

instance Functor action => Functor (Client up action)
  where
    fmap f = client_fmap_f
      where
        client_fmap_f = \case
          Pure product      ->  Pure (f product)
          Perform action    ->  Perform (fmap f action)
          Request request   ->  Bind (Request request) (Pure . f)
          Bind step1 step2  ->  Bind step1 (client_fmap_f . step2)

instance Functor m => Applicative (Client a m)
  where
    pure = Pure

    a1 <*> a2 = a1 `Bind` \f -> (f $) <$> a2
    a1  *> a2 = a1 `Bind` \_ ->           a2

instance Functor m => Monad (Client a m)
  where
    (>>=) = Bind

newtype Vendor (up :: Interface) (down :: Interface) (action :: Action) =
  Vendor
    { runVendor ::
        Client up action
          ( forall product.
              down product -> Client up action (Supply up down action product)
          )
    }

data Supply (up :: Interface) (down :: Interface) (action :: Action) (product :: Type) =
  Supply
    { supplyNext :: Vendor up down action
    , supplyProduct :: product
    }

deriving stock instance Functor (Supply up down action)

connectVendorToClient :: Functor m => Vendor a b m -> Client b m r -> Client a m (Supply a b m r)
connectVendorToClient up =
  \case
    Pure    x    ->  Pure                    Supply{ supplyNext = up, supplyProduct = x }
    Perform act  ->  Perform $ act <&> \x -> Supply{ supplyNext = up, supplyProduct = x }
    Bind    x f  ->  connectVendorToClient up x `Bind`
                       \Supply{ supplyNext = up', supplyProduct = y } ->
                          connectVendorToClient up' (f y)
    Request r    ->  connectVendorToRequest up r

connectVendorToRequest :: Vendor a b m -> b r -> Client a m (Supply a b m r)
connectVendorToRequest up =
  case runVendor up of
    Pure h      ->  h
    Perform x   ->  \r -> Perform x          `Bind` \h -> h r
    Request r'  ->  \r -> Request r'         `Bind` \h -> h r
    Bind x f    ->  \r -> x `Bind` \y -> f y `Bind` \h -> h r

connectVendorToVendor :: Functor m => Vendor a b m -> Vendor b c m -> Vendor a c m
connectVendorToVendor up (Vendor down) =
  Vendor $
    connectVendorToClient up down <&> \s r ->
      connectVendorToClient (supplyNext s) (supplyProduct s r) <&>
        supplyJoin

supplyJoin :: Functor action => Supply a b action (Supply b c action product) -> Supply a c action product
supplyJoin s =
  Supply
    { supplyNext = connectVendorToVendor (supplyNext s) (supplyNext (supplyProduct s))
    , supplyProduct = supplyProduct (supplyProduct s)
    }
