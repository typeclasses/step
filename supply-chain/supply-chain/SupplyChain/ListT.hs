module SupplyChain.ListT where

import SupplyChain
import SupplyChain.TerminableStream

import Control.Applicative (Applicative (..))
import Control.Monad (Monad (..), ap)
import Data.Function (($), (.))
import Data.Functor (Functor (..))
import Data.Functor.Const (Const (..))
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup (..))
import Data.Maybe (Maybe (..))
import Data.Void (Void)

newtype ListT m a =
  ListT
    { unListT :: Vendor (Const Void) (TerminableStream a) m
    }

instance Functor m => Semigroup (ListT m a)
  where
    ListT a <> ListT b = ListT (append a b)

instance Functor m => Monoid (ListT m a)
  where
    mempty = ListT nil

instance Functor m => Functor (ListT m)
  where
    fmap f (ListT v) = ListT (v >-> map f)

instance Functor m => Applicative (ListT m)
  where
    pure x = ListT $ Vendor \NextMaybe -> pure $ Just x :-> nil
    (<*>) = ap

instance Functor m => Monad (ListT m)
  where
    ListT v1 >>= f = ListT (v1 >-> concatMapVendor (unListT . f))
