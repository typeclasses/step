module SupplyChain.Interface.TerminableStream
  (
    {- * Interface -} TerminableStream (..),
    {- * Vendors -} nil, singleton, list, map, concatMap, concat, while, group,
    {- * Vendor composition -} append, concatMapVendor,
    {- * Clients -} all,
  )
  where

import SupplyChain

import Control.Applicative (pure)
import Control.Monad ((>>=))
import Data.Bool (Bool (..))
import Data.Eq (Eq, (==))
import Data.Function (($), id)
import Data.Functor (Functor, (<$>), (<&>))
import Data.Functor.Const (Const (..))
import Data.Kind (Type)
import Data.Maybe (Maybe (..))
import Data.Void (Void)
import Numeric.Natural (Natural)
import Prelude ((+))


data TerminableStream item response =
    (response ~ Maybe item) => NextMaybe
        -- ^ The next item, or 'Nothing' if input is exhausted
        --
        -- It is assumed that after a 'Nothing' response is given,
        -- all subsequent responses will also be 'Nothing'.

type TerminableStream :: Type -> Interface


-- | Yields each item from the list, then stops

list :: forall up a action. Functor action =>
    [a] -> Vendor up (TerminableStream a) action

list = go
  where
    go :: [a] -> Vendor up (TerminableStream a) action
    go = \case
        []      ->  nil
        x : xs  ->  Vendor \NextMaybe -> pure (Just x :-> go xs)


-- | The empty stream

nil :: forall up a action. Functor action =>
    Vendor up (TerminableStream a) action

nil = go
  where
    go :: Vendor up (TerminableStream a) action
    go = Vendor \NextMaybe -> pure (Nothing :-> go)


-- | Yields one item, then stops

singleton :: forall up a action. Functor action =>
    a -> Vendor up (TerminableStream a) action

singleton x = Vendor \NextMaybe -> pure (Just x :-> nil)


-- | Apply a function to each item in the stream

map :: forall a b action. Functor action =>
    (a -> b) -> Vendor (TerminableStream a) (TerminableStream b) action

map f = Vendor \NextMaybe -> order NextMaybe <&> \case
    Nothing -> Nothing    :-> nil
    Just a  -> Just (f a) :-> map f


{-| Applies the function to each result obtained from upstream,
    and yields each result from the list to the downstream
-}

concatMap :: forall a b action. Functor action =>
    (a -> [b]) -> Vendor (TerminableStream a) (TerminableStream b) action

concatMap f = Vendor \NextMaybe -> go []
  where
    go :: [b] -> Client (TerminableStream a) action
        (Supply (TerminableStream a) (TerminableStream b) action (Maybe b))
    go = \case
        b : bs' -> pure (Just b :-> Vendor \NextMaybe -> go bs')
        [] -> order NextMaybe >>= \case
            Nothing -> pure (Nothing :-> nil)
            Just a  -> go (f a)


-- | Like 'concatMap', but the function gives a vendor instead of a list

concatMapVendor :: forall a b action. Functor action =>
    (a -> Vendor (Const Void) (TerminableStream b) action)
    -> Vendor (TerminableStream a) (TerminableStream b) action

concatMapVendor f = go
  where
    go = Vendor \NextMaybe -> order NextMaybe >>= \case
        Nothing -> pure $ Nothing :-> nil
        Just x -> offer (append (noVendor >-> f x) go) NextMaybe


-- | Flattens a stream of lists

concat :: forall a action. Functor action =>
    Vendor (TerminableStream [a]) (TerminableStream a) action

concat = concatMap id


-- | Yields the longest stream prefix matching the predicate

while :: forall a action. Functor action =>
    (a -> Bool)
    -> Vendor (TerminableStream a) (TerminableStream a) action

while ok = v
  where
    v = Vendor \NextMaybe ->
        order NextMaybe >>= \case
            Just x | ok x  ->  pure $ Just x  :-> v
            _              ->  pure $ Nothing :-> nil


{-| Yields all the items of the first stream,
    followed by all the items of the second
-}

append :: forall up a action. Functor action =>
       Vendor up (TerminableStream a) action
    -> Vendor up (TerminableStream a) action
    -> Vendor up (TerminableStream a) action

append a b = Vendor \NextMaybe ->
    offer a NextMaybe >>= \case
        Nothing :-> _ -> offer b NextMaybe
        Just x :-> a' -> pure $ Just x :-> append a' b


-- | Collects everything from the stream

all :: forall a action. Functor action =>
    Client (TerminableStream a) action [a]

all = go
  where
    go = order NextMaybe >>= \case
        Nothing -> pure []
        Just x -> (x :) <$> go


group :: forall a action. Eq a => Functor action =>
    Vendor (TerminableStream a) (TerminableStream (Natural, a)) action
group = Vendor \NextMaybe ->
    order NextMaybe >>= \case
        Nothing -> pure $ Nothing :-> nil
        Just x -> go 1 x
  where
    go :: Natural -> a -> Client (TerminableStream a) action
        ( Supply (TerminableStream a) (TerminableStream (Natural, a))
          action (Maybe (Natural, a))
        )
    go n x = order NextMaybe >>= \case
        Nothing -> pure $ Just (n, x) :-> nil
        Just y ->
          if y == x
          then go (n + 1) x
          else pure $ Just (n, x) :-> Vendor \NextMaybe -> go 1 y
