module ActionList
  (
    {- * Type         -}  ActionList (..),
    {- * Introduction -}  fromList, perform,
    {- * Elimination  -}  toList, runActionList,
    {- * Use          -}  next,
  )
  where

import SupplyChain ( Vendor, Referral (Referral), (>->), (>-))
import qualified SupplyChain.Vendor as Vendor
import qualified SupplyChain.Job as Job

import SupplyChain.Interface.TerminableStream (TerminableStream)
import qualified SupplyChain.Interface.TerminableStream as Stream

import Control.Applicative (Applicative, pure, (<*>))
import Data.Maybe (Maybe)
import Control.Monad (Monad, (>>=), ap)
import Data.Function ((.), ($), (&))
import Data.Functor (Functor, fmap)
import Data.Monoid (Monoid, mempty)
import Data.Semigroup (Semigroup, (<>))
import Data.Traversable (sequence)
import Data.Functor.Const (Const)
import Data.Void (Void)

newtype ActionList m a =
  VendorActionList
    { actionListVendor :: Vendor (Const Void) (TerminableStream a) m
    }

instance Semigroup (ActionList m a)
  where
    VendorActionList a <> VendorActionList b =
        VendorActionList (Stream.append a b)

instance Monoid (ActionList m a)
  where
    mempty = VendorActionList Stream.nil

instance Functor (ActionList m)
  where
    fmap f (VendorActionList v) =
        VendorActionList (v >-> Stream.map f)

instance Applicative (ActionList m)
  where
    pure = VendorActionList . Stream.singleton
    (<*>) = ap

instance Monad (ActionList m)
  where
    VendorActionList v1 >>= f = VendorActionList $
        v1 >-> Stream.concatMapVendor (actionListVendor . f)

-- | Converts an ordinary list into an 'ActionList'
fromList :: forall m a. [a] -> ActionList m a
fromList = VendorActionList . Stream.list

-- | A singleton list where the item is obtained by performing an action
perform :: forall m a. m a -> ActionList m a
perform = VendorActionList . Stream.actionSingleton

-- | Converts an 'ActionList' into an ordinary list
toList :: forall a. ActionList (Const Void) a -> [a]
toList xs = Job.eval $ actionListVendor xs >- Stream.all

-- | Converts an 'ActionList' into an action that returns all the items at once
runActionList :: forall m a. Monad m => ActionList m a -> m [a]
runActionList xs = Job.run $ actionListVendor xs >- Stream.all

next :: forall m a. Monad m => ActionList m a -> m (Maybe (a, ActionList m a))
next xs = fmap @m f $ Vendor.run (actionListVendor xs) Stream.NextMaybe
  where
    f :: Referral (Const Void) (TerminableStream a) m (Maybe a) -> Maybe (a, ActionList m a)
    f s = s & sequence & fmap @Maybe \(Referral x v) -> (x, VendorActionList v)
