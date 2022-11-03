{- |

Description: A streaming list type based on the 'TerminableStream' interface

-}

module ActionList
  (
    {- * Type -} ActionList (..),
    {- * Introduction -} fromList, perform,
    {- * Elimination -} toList, runActionList,
    {- * Use -} next,
  )
  where

import SupplyChain (Connect ((>->)), Vendor, evalJob, runJob, runVendor, vendorToJob, vendorToVendor, NoInterface, NoAction, Supply (..))
import SupplyChain.Interface.TerminableStream (TerminableStream)
import qualified SupplyChain.Interface.TerminableStream as Stream

import Control.Applicative (Applicative (..))
import Data.Maybe (Maybe (..))
import Control.Monad (Monad (..), ap)
import Data.Function ((.), ($))
import Data.Functor (Functor (..), (<&>))
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup (..))

newtype ActionList m a =
  VendorActionList
    { actionListVendor :: Vendor NoInterface (TerminableStream a) m
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
    VendorActionList v1 >>= f =
        VendorActionList (vendorToVendor v1 $ Stream.concatMapVendor (actionListVendor . f))

-- | Converts an ordinary list into an 'ActionList'
fromList :: [a] -> ActionList m a
fromList = VendorActionList . Stream.list

-- | A singleton list where the item is obtained by performing an action
perform :: m a -> ActionList m a
perform x = VendorActionList (Stream.actionSingleton x)

-- | Converts an 'ActionList' into an ordinary list
toList :: ActionList NoAction a -> [a]
toList (VendorActionList v) = evalJob (vendorToJob v Stream.all)

-- | Converts an 'ActionList' into an action that returns all the items at once
runActionList :: Monad m => ActionList m a -> m [a]
runActionList (VendorActionList v) = runJob (vendorToJob v Stream.all)

next :: Monad m => ActionList m a -> m (Maybe (a, ActionList m a))
next (VendorActionList v) =
    SupplyChain.runVendor v Stream.NextMaybe <&> \(Supply xm v') ->
        xm <&> (, VendorActionList v')
