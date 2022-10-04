module SupplyChain.Core where

import Control.Applicative (Applicative (pure, (*>), (<*>)), (<$>))
import Control.Monad (Monad ((>>=)), Functor (fmap))
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

instance Functor action => Applicative (Client up action)
  where
    pure = Pure

    a1 <*> a2 = a1 `Bind` \f -> (f $) <$> a2
    a1  *> a2 = a1 `Bind` \_ ->           a2

instance Functor action => Monad (Client up action)
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


connectVendorToClient :: forall up down action product. Functor action =>
    Vendor up down action -> Client down action product -> Client up action (Supply up down action product)

connectVendorToClient vendor =
  \case
    Pure product      ->  Pure Supply{ supplyNext = vendor, supplyProduct = product }
    Perform action    ->  Perform (action <&> \product -> Supply{ supplyNext = vendor, supplyProduct = product })
    Request request   ->  connectVendorToRequest vendor request
    Bind step1 step2  ->  connectVendorToClient vendor step1 `Bind` \supply ->
                            connectVendorToClient (supplyNext supply) (step2 (supplyProduct supply))

connectVendorToRequest :: forall up down action product.
    Vendor up down action -> down product -> Client up action (Supply up down action product)

connectVendorToRequest up =
  case runVendor up of
    Pure handle       ->  \request ->                                              handle request
    Perform action    ->  \request -> Perform action             `Bind` \handle -> handle request
    Request request'  ->  \request -> Request request'           `Bind` \handle -> handle request
    Bind step1 step2  ->  \request -> step1 `Bind` \x -> step2 x `Bind` \handle -> handle request


connectVendorToVendor :: forall up middle down action. Functor action =>
    Vendor up middle action -> Vendor middle down action -> Vendor up down action

connectVendorToVendor up (Vendor down) =
  Vendor $
    connectVendorToClient up down <&> \supply request ->
      connectVendorToClient (supplyNext supply) (supplyProduct supply request) <&>
        supplyJoin


supplyJoin :: forall up middle down action product. Functor action =>
    Supply up middle action (Supply middle down action product) -> Supply up down action product

supplyJoin s =
  Supply
    { supplyNext = connectVendorToVendor (supplyNext s) (supplyNext (supplyProduct s))
    , supplyProduct = supplyProduct (supplyProduct s)
    }
