module SupplyChain.More
  (
    module SupplyChain.Base,
    {- * Void request -} Nil, nil,
    {- * Combining vendors -} Either' (..), bivend,
    {- * Trivial vendors -} function,
    {- * Counting -} Counting (..), counting,
    {- * Infinite streams -} InfiniteStream (..), iterate,
    {- * Finite streams -} FiniteStream (..), list,
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

---

-- | Used as the upstream interface for a vendor or client that does not make any requests
data Nil product

type Nil :: Interface

nil :: Nil product -> product
nil = \case{}

---

type Either' :: Interface -> Interface -> Interface

-- | Combination of two interfaces
data Either' a b product = Left' (a product) | Right' (b product)

-- | Combination of two vendors
bivend :: Functor m => Vendor u a m -> Vendor u b m -> Vendor u (Either' a b) m
bivend a b = vend $ pure \case
    Left'  req -> runVendor a >>= \f -> f req <&> \s -> s{ supplyNext = bivend (supplyNext s) b }
    Right' req -> runVendor b >>= \f -> f req <&> \s -> s{ supplyNext = bivend a (supplyNext s) }

---

-- | A simple stateless vendor that responds to each request by applying a function
function :: Functor m => (forall product. i product -> product) -> Vendor up i m
function f = go
  where
    go = vend $ pure \x -> pure $ f x +> go

---

type InfiniteStream :: Type -> Interface

data InfiniteStream item product =
    (item ~ product) => Next
        -- ^ The next item from a nonterminating input stream

iterate :: forall a m up. Functor m => a -> (a -> a) -> Vendor up (InfiniteStream a) m
iterate = flip it
  where
    it f = go
      where
        go :: a -> Vendor up (InfiniteStream a) m
        go x = vend $ pure \Next -> pure $ x +> go (f x)

---

type FiniteStream :: Type -> Interface

data FiniteStream item product =
    (product ~ Maybe item) => NextMaybe
        -- ^ The next item, or 'Nothing' if input is exhausted

list :: forall a m up. Functor m => [a] -> Vendor up (FiniteStream a) m
list = go
  where
    go :: [a] -> Vendor up (FiniteStream a) m
    go = \case
        []      ->  endOfList
        x : xs  ->  vend $ pure $ \NextMaybe -> pure $ Just x +> go xs

endOfList :: Functor m => Vendor up (FiniteStream a) m
endOfList = go
  where
    go = vend $ pure \NextMaybe -> pure $ Nothing +> go

---

type Counting :: Interface -> Interface

data Counting i product =
    Counting_order (i product)
        -- ^ The next item, or 'Nothing' if input is exhausted
  | (product ~ Natural) => Counting_count
      -- ^ How many items have been fetched so far

counting :: forall i m. Functor m => Vendor i (Counting i) m
counting = go 0
  where
    go :: Natural -> Vendor i (Counting i) m
    go n = vend $ pure \case
        Counting_count   ->  pure $ n +> go n
        Counting_order x  ->  order x <&> (+> go (n + 1))
