module SupplyChain.Extra.List where

import SupplyChain
import SupplyChain.Interface.TerminableStream

import Control.Applicative (Applicative (..))
import Control.Monad (Monad (..), ap)
import Data.Function (($), (.))
import Data.Functor (Functor (..))
import Data.Functor.Const (Const (..))
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup (..))
import Data.Maybe (Maybe (..))
import Data.Void (Void)

newtype List m a =
  List
    { listVendor :: Vendor (Const Void) (TerminableStream a) m
    }

instance Functor m => Semigroup (List m a)
  where
    List a <> List b = List (append a b)

instance Functor m => Monoid (List m a)
  where
    mempty = List nil

instance Functor m => Functor (List m)
  where
    fmap f (List v) = List (v >-> map f)

instance Functor m => Applicative (List m)
  where
    pure x = List $ Vendor \NextMaybe -> pure $ Just x :-> nil
    (<*>) = ap

instance Functor m => Monad (List m)
  where
    List v1 >>= f = List (v1 >-> concatMapVendor (listVendor . f))
