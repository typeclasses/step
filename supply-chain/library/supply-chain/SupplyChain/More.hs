module SupplyChain.More
  (
    module SupplyChain.Base,
    {- * Void request -} Nil, nil,
    {- * Combining vendors -} Either' (..), bivend,
    {- * Trivial vendors -} functionVendor,
    {- * Counting -} Counting (..), countingVendor,
    {- * Working with finite streams -} Next (..), Vitality (..),
  )
  where

import SupplyChain.Base

import Control.Applicative
import Control.Monad
import Data.Function
import Data.Functor
import Data.Functor.Identity
import Data.Kind
import Data.Maybe
import Numeric.Natural
import Prelude ((+))

-- | Used as the upstream interface for a vendor or client that does not make any requests
data Nil r

type Nil :: I

nil :: Nil r -> r
nil = \case{}

data Either' a b r = Left' (a r) | Right' (b r)

bivend :: Functor m => Vendor u a m -> Vendor u b m -> Vendor u (Either' a b) m
bivend a b = vend $ pure \case
    Left'  req -> runVendor a >>= \f -> f req <&> \s -> s{ next = bivend (next s) b }
    Right' req -> runVendor b >>= \f -> f req <&> \s -> s{ next = bivend a (next s) }

functionVendor :: Functor m => (forall r. a r -> r) -> Vendor Nil a m
functionVendor f = go
  where
    go = vend $ pure \x -> pure $ f x +> go

data Next a
  where
    Next :: Next (Maybe a)

type Next :: I

data Counting item a
  where
    Counting_next :: Counting item (Maybe item)
      -- ^ The next item
    Counting_count :: Counting item Natural
      -- ^ How many items have been fetched so far

type Counting :: Type -> I

data Vitality = Live | Dead

countingVendor :: forall a m. Applicative m => Vendor Next (Counting a) m
countingVendor = go Live 0
  where
    go :: Vitality -> Natural -> Vendor Next (Counting a) m
    go v n = vend $ pure \case
        Counting_count -> pure $ n +> go v n
        Counting_next  ->
            request Next >>= \case
                Nothing -> pure $ Nothing +> go Dead n
                Just x  -> pure $ Just x  +> go Live (n + 1)
