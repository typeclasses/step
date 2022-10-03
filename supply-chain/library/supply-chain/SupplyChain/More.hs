module SupplyChain.More
  (
    module SupplyChain.Base,
    {- * Void request -} Nil, nil,
    {- * Combining vendors -} Either' (..), bivend,
    {- * Trivial vendors -} function,
    {- * Counting -} Counting (..), counting,
    {- * Infinite streams -} Next (..), iterate,
    {- * Finite streams -} NextMaybe (..), Vitality (..), list,
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
data Nil product

type Nil :: Interface

nil :: Nil product -> product
nil = \case{}

data Either' a b product = Left' (a product) | Right' (b product)

bivend :: Functor m => Vendor u a m -> Vendor u b m -> Vendor u (Either' a b) m
bivend a b = vend $ pure \case
    Left'  req -> runVendor a >>= \f -> f req <&> \s -> s{ next = bivend (next s) b }
    Right' req -> runVendor b >>= \f -> f req <&> \s -> s{ next = bivend a (next s) }

function :: Functor m => (forall product. a product -> product) -> Vendor up a m
function f = go  where  go = vend $ pure \x -> pure $ f x +> go

data Next item product
  where
    Next :: Next item item
      -- ^ The next item from a nonterminating input stream

iterate :: forall a m up. Functor m => a -> (a -> a) -> Vendor up (Next a) m
iterate = flip it
  where
    it f = go
      where
        go :: a -> Vendor up (Next a) m
        go x = vend $ pure \Next -> pure $ x +> go (f x)

data NextMaybe item product
  where
    NextMaybe :: NextMaybe item (Maybe item)
      -- ^ The next item, or 'Nothing' if input is exhausted

type NextMaybe :: Type -> Interface

data Counting item product
  where
    Counting_next :: Counting item (Maybe item)
      -- ^ The next item, or 'Nothing' if input is exhausted
    Counting_count :: Counting item Natural
      -- ^ How many items have been fetched so far

list :: forall a m up. Functor m => [a] -> Vendor up (NextMaybe a) m
list = go
  where
    go :: [a] -> Vendor up (NextMaybe a) m
    go = \case
        []      ->  endOfList
        x : xs  ->  vend $ pure $ \NextMaybe -> pure $ Just x +> go xs

endOfList :: Functor m => Vendor up (NextMaybe a) m
endOfList = go
  where
    go = vend $ pure \NextMaybe -> pure $ Nothing +> go

type Counting :: Type -> Interface

data Vitality = Live | Dead

counting :: forall a m. Functor m => Vendor (NextMaybe a) (Counting a) m
counting = go Live 0
  where
    go :: Vitality -> Natural -> Vendor (NextMaybe a) (Counting a) m
    go v n = vend $ pure \case
        Counting_count  ->  pure $ n +> go v n
        Counting_next   ->
            request NextMaybe <&> \case
                Nothing  ->  Nothing +> go Dead n
                Just x   ->  Just x  +> go Live (n + 1)
