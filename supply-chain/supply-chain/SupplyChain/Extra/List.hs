module SupplyChain.Extra.List where

import SupplyChain
import SupplyChain.Interface.TerminableStream (TerminableStream)
import qualified SupplyChain.Interface.TerminableStream as Stream

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

instance Semigroup (List m a)
  where
    VendorList a <> VendorList b = VendorList (Stream.append a b)

instance Monoid (List m a)
  where
    mempty = VendorList Stream.nil

instance Functor (List m)
  where
    fmap f (VendorList v) = VendorList (v >-> Stream.map f)

instance Applicative (List m)
  where
    pure = VendorList . Stream.singleton
    (<*>) = ap

instance Monad (List m)
  where
    VendorList v1 >>= f = VendorList (v1 >-> Stream.concatMapVendor (listVendor . f))

fromList :: [a] -> List m a
fromList = VendorList . Stream.list

toList :: List Identity a -> [a]
toList (VendorList v) = eval (v >-> Stream.all)

toListM :: Monad m => List m a -> m [a]
toListM (VendorList v) = run (v >-> Stream.all)
