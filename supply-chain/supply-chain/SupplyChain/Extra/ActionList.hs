{- |

Description: A streaming 'List' type based on the 'TerminableStream' interface

-}

module SupplyChain.Extra.ActionList
  (
    {- * Type -} ActionList (..),
    {- * Introduction -} fromList,
    {- * Elimination -} toList, toListM,
  )
  where

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
    VendorActionList v1 >>= f =
        VendorActionList (v1 >-> Stream.concatMapVendor (actionListVendor . f))

fromList :: [a] -> ActionList m a
fromList = VendorActionList . Stream.list

toList :: ActionList Identity a -> [a]
toList (VendorActionList v) = eval (v >-> Stream.all)

toListM :: Monad m => ActionList m a -> m [a]
toListM (VendorActionList v) = run (v >-> Stream.all)
