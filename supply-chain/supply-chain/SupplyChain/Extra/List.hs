module SupplyChain.Extra.List where

import SupplyChain
import SupplyChain.Interface.TerminableStream

import Control.Applicative (Applicative (..))
import Control.Monad (Monad (..), ap)
import Data.Function ((.))
import Data.Functor (Functor (..))
import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup (..))
import Data.Void (Void)

newtype List m a =
  VendorList
    { listVendor :: Vendor (Const Void) (TerminableStream a) m
    }

instance Functor m => Semigroup (List m a)
  where
    VendorList a <> VendorList b = VendorList (append a b)

instance Functor m => Monoid (List m a)
  where
    mempty = VendorList nil

instance Functor m => Functor (List m)
  where
    fmap f (VendorList v) = VendorList (v >-> map f)

instance Functor m => Applicative (List m)
  where
    pure = VendorList . singleton
    (<*>) = ap

instance Functor m => Monad (List m)
  where
    VendorList v1 >>= f = VendorList (v1 >-> concatMapVendor (listVendor . f))

fromList :: Functor m => [a] -> List m a
fromList = VendorList . list

toList :: List Identity a -> [a]
toList (VendorList v) = eval (v >-> all)

toListM :: Monad m => List m a -> m [a]
toListM (VendorList v) = run (v >-> all)
