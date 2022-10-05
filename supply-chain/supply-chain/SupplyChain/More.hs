module SupplyChain.More
  (
    module SupplyChain.Base,

    {- * Eval              -}  eval,
    {- * Void request      -}  Nil, nil,
    {- * Combining vendors -}  Either' (..), offerEither,
    {- * Trivial vendors   -}  function,
    {- * Counting          -}  Counting (..), counting,
    {- * Infinite streams  -}  InfiniteStream (..), iterate,
    {- * Finite streams    -}  FiniteStream (..), list, while,

  )
  where

import SupplyChain.Base

import Control.Applicative (pure)
import Control.Monad ((>>=))
import Data.Bool (Bool (..))
import Data.Function ((.), ($), flip)
import Data.Functor (Functor, (<&>))
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.Maybe (Maybe (..))
import Numeric.Natural (Natural)
import Prelude ((+))

---

-- | Use this instead of 'run' when @action = 'Identity'@
eval :: forall (up :: Interface) (product :: Type).
    (forall x. up x -> x) -> Client up Identity product -> product
eval f = runIdentity . run (pure . f)

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

type InfiniteStream :: Type -> Interface

data InfiniteStream item response =
    (response ~ item) => Next
        -- ^ The next item from a non-terminating input stream

iterate :: forall up a action. Functor action =>
    a -> (a -> a) -> Vendor up (InfiniteStream a) action
iterate = flip it
  where
    it f = go
      where
        go :: a -> Vendor up (InfiniteStream a) action
        go x = Vendor \Next -> pure $ x :-> go (f x)

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
    go n = Vendor \case
        Counting_count    ->  pure $ n :-> go n
        Counting_order x  ->  order x <&> (:-> go (n + 1))
