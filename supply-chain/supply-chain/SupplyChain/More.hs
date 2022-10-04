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
data Nil response

type Nil :: Interface

nil :: Nil response -> response
nil = \case{}

---

type Either' :: Interface -> Interface -> Interface

-- | Combination of two interfaces
data Either' a b response = Left' (a response) | Right' (b response)

-- | Combination of two vendors
bivend :: forall up down1 down2 action. Functor action =>
    Vendor up down1 action -> Vendor up down2 action -> Vendor up (Either' down1 down2) action
bivend a b = vend $ pure \case
    Left'  req -> runVendor a >>= \f -> f req <&> \s -> s{ supplyNext = bivend (supplyNext s) b }
    Right' req -> runVendor b >>= \f -> f req <&> \s -> s{ supplyNext = bivend a (supplyNext s) }

---

-- | A simple stateless vendor that responds to each request by applying a function
function :: forall up down action. Functor action =>
    (forall response. down response -> response) -> Vendor up down action
function f = go
  where
    go = vend $ pure \x -> pure $ f x +> go

---

type InfiniteStream :: Type -> Interface

data InfiniteStream item response =
    (response ~ item) => Next
        -- ^ The next item from a nonterminating input stream

iterate :: forall up a action. Functor action =>
    a -> (a -> a) -> Vendor up (InfiniteStream a) action
iterate = flip it
  where
    it f = go
      where
        go :: a -> Vendor up (InfiniteStream a) action
        go x = vend $ pure \Next -> pure $ x +> go (f x)

---

type FiniteStream :: Type -> Interface

data FiniteStream item response =
    (response ~ Maybe item) => NextMaybe
        -- ^ The next item, or 'Nothing' if input is exhausted

list :: forall up a action. Functor action =>
    [a] -> Vendor up (FiniteStream a) action
list = go
  where
    go :: [a] -> Vendor up (FiniteStream a) action
    go = \case
        []      ->  endOfList
        x : xs  ->  vend $ pure $ \NextMaybe -> pure $ Just x +> go xs

endOfList :: forall up a action. Functor action =>
    Vendor up (FiniteStream a) action
endOfList = go
  where
    go = vend $ pure \NextMaybe -> pure $ Nothing +> go

---

type Counting :: Interface -> Interface

data Counting i response =
    Counting_order (i response)
        -- ^ The next item, or 'Nothing' if input is exhausted
  | (response ~ Natural) => Counting_count
        -- ^ How many items have been fetched so far

counting :: forall i action. Functor action => Vendor i (Counting i) action
counting = go 0
  where
    go :: Natural -> Vendor i (Counting i) action
    go n = vend $ pure \case
        Counting_count   ->  pure $ n +> go n
        Counting_order x  ->  order x <&> (+> go (n + 1))
