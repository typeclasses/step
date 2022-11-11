module ActionList
  (
    {- * Type         -}  ActionList (..),
    {- * Introduction -}  fromList, perform,
    {- * Elimination  -}  toList, runActionList,
    {- * Use          -}  next,
  )
  where

import qualified Internal

import SupplyChain (Vendor, Referral (Referral), Unit (Unit), (>->), (>-))
import qualified SupplyChain.Vendor as Vendor
import qualified SupplyChain.Job as Job

import Control.Applicative (Applicative, pure, (<*>))
import Control.Monad (Monad, (>>=), ap)
import Data.Function ((.), ($), (&))
import Data.Functor (Functor, fmap)
import Data.Functor.Const (Const)
import Data.Maybe (Maybe)
import Data.Monoid (Monoid, mempty)
import Data.Semigroup (Semigroup, (<>))
import Data.Traversable (sequence)
import Data.Void (Void)

newtype ActionList m a =
  VendorActionList
    { actionListVendor :: Vendor (Const Void) (Unit (Maybe a)) m
    }

instance Semigroup (ActionList m a)
  where
    VendorActionList a <> VendorActionList b =
        VendorActionList (Internal.append a b)

instance Monoid (ActionList m a)
  where
    mempty = VendorActionList Internal.nil

instance Functor (ActionList m)
  where
    fmap f (VendorActionList v) =
        VendorActionList (v >-> Internal.map f)

instance Applicative (ActionList m)
  where
    pure = VendorActionList . Internal.singleton
    (<*>) = ap

instance Monad (ActionList m)
  where
    VendorActionList v1 >>= f = VendorActionList $
        v1 >-> Internal.concatMapVendor (actionListVendor . f)

-- | Converts an ordinary list into an 'ActionList'
fromList :: forall m a. [a] -> ActionList m a
fromList = VendorActionList . Internal.list

-- | A singleton list where the item is obtained by performing an action
perform :: forall m a. m a -> ActionList m a
perform = VendorActionList . Internal.actionSingleton

-- | Converts an 'ActionList' into an ordinary list
toList :: forall a. ActionList (Const Void) a -> [a]
toList xs = Job.eval $ actionListVendor xs >- Internal.all

-- | Converts an 'ActionList' into an action that returns all the items at once
runActionList :: forall m a. Monad m => ActionList m a -> m [a]
runActionList xs = Job.run $ actionListVendor xs >- Internal.all

next :: forall m a. Monad m => ActionList m a -> m (Maybe (a, ActionList m a))
next xs = fmap @m f $ Vendor.run (actionListVendor xs) Unit
  where
    f :: Referral (Const Void) (Unit (Maybe a)) m (Maybe a) -> Maybe (a, ActionList m a)
    f s = s & sequence & fmap @Maybe (\(Referral x v) -> (x, VendorActionList v))
