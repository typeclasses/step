module SupplyChain.More
  (
    module SupplyChain.Base,

    {- * Combining vendors -}  Either' (..), offerEither,
    {- * Trivial vendors   -}  function,
    {- * Counting          -}  Counting (..), counting,
    {- * Infinite streams  -}  InfiniteStream (..), iterate, infiniteConcatMap, infiniteConcat,
    {- * Finite streams    -}  FiniteStream (..), list, while, finiteConcatMap, finiteConcat,

  )
  where

import SupplyChain.Base

import Control.Applicative (pure)
import Control.Monad ((>>=))
import Data.Bool (Bool (..))
import Data.Function (($), flip, id)
import Data.Functor (Functor, (<&>))
import Data.Kind (Type)
import Data.Maybe (Maybe (..))
import Numeric.Natural (Natural)
import Prelude ((+))

---

-- | Combination of two interfaces

data Either' a b response = Left' (a response) | Right' (b response)

type Either' :: Interface -> Interface -> Interface


-- | Combination of two vendors

offerEither :: forall up down1 down2 action. Functor action =>
    Vendor up down1 action -> Vendor up down2 action -> Vendor up (Either' down1 down2) action

offerEither a@(Vendor a') b@(Vendor b') = Vendor \case
    Left'  req -> a' req <&> \s -> s{ supplyNext = offerEither (supplyNext s) b }
    Right' req -> b' req <&> \s -> s{ supplyNext = offerEither a (supplyNext s) }


---


-- | A simple stateless vendor that responds to each request by applying a function

function :: forall up down action. Functor action =>
    (forall response. down response -> response) -> Vendor up down action

function f = go
  where
    go = Vendor \x -> pure $ f x :-> go


---

data InfiniteStream item response =
    (response ~ item) => Next
        -- ^ The next item from a non-terminating input stream

type InfiniteStream :: Type -> Interface


iterate :: forall up a action. Functor action =>
    a -> (a -> a) -> Vendor up (InfiniteStream a) action

iterate = flip it
  where
    it f = go
      where
        go :: a -> Vendor up (InfiniteStream a) action
        go x = Vendor \Next -> pure $ x :-> go (f x)


infiniteConcatMap :: forall a b action. Functor action =>
    (a -> [b]) -> Vendor (InfiniteStream a) (InfiniteStream b) action

infiniteConcatMap f = go []
  where
    go :: [b] -> Vendor (InfiniteStream a) (InfiniteStream b) action
    go bs = Vendor \Next -> case bs of
        b : bs' -> pure $ b :-> go bs'
        [] -> order Next >>= \a -> offer (go (f a)) Next


infiniteConcat :: forall a action. Functor action =>
    Vendor (InfiniteStream [a]) (InfiniteStream a) action

infiniteConcat = infiniteConcatMap id


---

data FiniteStream item response =
    (response ~ Maybe item) => NextMaybe
        -- ^ The next item, or 'Nothing' if input is exhausted

type FiniteStream :: Type -> Interface


list :: forall up a action. Functor action =>
    [a] -> Vendor up (FiniteStream a) action

list = go
  where
    go :: [a] -> Vendor up (FiniteStream a) action
    go = \case
        []      ->  endOfList
        x : xs  ->  Vendor \NextMaybe -> pure $ Just x :-> go xs


endOfList :: forall up a action. Functor action =>
    Vendor up (FiniteStream a) action

endOfList = go
  where
    go :: Vendor up (FiniteStream a) action
    go = Vendor \NextMaybe -> pure $ Nothing :-> go


while :: forall a action. Functor action =>
    (a -> Bool)
    -> Vendor (FiniteStream a) (FiniteStream a) action

while ok = v
  where
    v = Vendor \NextMaybe ->
        order NextMaybe >>= \case
            Just x | ok x  ->  pure $ Just x  :-> v
            _              ->  pure $ Nothing :-> endOfList


finiteConcatMap :: forall a b action. Functor action =>
    (a -> [b]) -> Vendor (FiniteStream a) (FiniteStream b) action

finiteConcatMap f = go []
  where
    go :: [b] -> Vendor (FiniteStream a) (FiniteStream b) action
    go bs = Vendor \NextMaybe -> case bs of
        b : bs' -> pure $ Just b :-> go bs'
        [] -> order NextMaybe >>= \case
            Nothing -> pure $ Nothing :-> endOfList
            Just a -> offer (go (f a)) NextMaybe

finiteConcat :: forall a action. Functor action =>
    Vendor (FiniteStream [a]) (FiniteStream a) action

finiteConcat = finiteConcatMap id


---

data Counting i response =
    Counting_order (i response)
        -- ^ The next item, or 'Nothing' if input is exhausted
  | (response ~ Natural) => Counting_count
        -- ^ How many items have been fetched so far

type Counting :: Interface -> Interface


counting :: forall i action. Functor action =>
    Vendor i (Counting i) action

counting = go 0
  where
    go :: Natural -> Vendor i (Counting i) action
    go n = Vendor \case
        Counting_count    ->  pure $ n :-> go n
        Counting_order x  ->  order x <&> (:-> go (n + 1))
