{-|

Description:
    Like 'TerminableStream', but with an additional operation
    for taking the entire remainder

-}

module SupplyChain.Interface.DrainableStream where

import SupplyChain hiding (map)
import SupplyChain.Interface.TerminableStream (IsTerminableStream (..))

import Control.Applicative (pure)
import Data.Functor (Functor, (<&>), fmap)
import Data.Kind (Type)
import Data.Maybe (Maybe (..))

import qualified Data.List as List


class Functor list => List list
  where
    emptyList :: list a
    singletonList :: a -> list a
    unconsList :: list a -> Maybe (a, list a)


instance List []
  where
    emptyList = []
    singletonList = (: [])
    unconsList = List.uncons


data DrainableStream (list :: Type -> Type) item response =
    (response ~ Maybe item) => NextMaybe
        -- ^ The next item, or 'Nothing' if input is exhausted
        --
        -- It is assumed that after a 'Nothing' response is given,
        -- all subsequent responses will also be 'Nothing'.
  | (response ~ list item) => Drain

type DrainableStream :: (Type -> Type) -> Type -> Interface


instance IsTerminableStream item (DrainableStream list item)
  where
    nextMaybe = NextMaybe


-- | Yields each item from the list, then stops

list :: forall up list a action. List list =>
    list a
    -> Vendor up (DrainableStream list a) action

list = go
  where
    go :: list a -> Vendor up (DrainableStream list a) action
    go xs = case unconsList xs of
        Nothing -> nil
        Just (x, xs') -> Vendor \case
            NextMaybe -> pure (Just x :-> go xs')
            Drain     -> pure (xs     :-> nil)


-- | The empty stream

nil :: forall up list a action. List list =>
    Vendor up (DrainableStream list a) action

nil = go
  where
    go :: Vendor up (DrainableStream list a) action
    go = Vendor \case
        NextMaybe -> pure (Nothing   :-> go)
        Drain     -> pure (emptyList :-> go)


-- | Yields one item, then stops

singleton :: forall up list a action. List list =>
    a -> Vendor up (DrainableStream list a) action

singleton x = Vendor \case
    NextMaybe -> pure (Just x          :-> nil)
    Drain     -> pure (singletonList x :-> nil)


-- | Performs one action, yields the resulting item, then stops

actionSingleton :: forall up list a action. List list =>
    action a -> Vendor up (DrainableStream list a) action

actionSingleton mx =
    Vendor \case
        NextMaybe -> perform mx <&> \x -> Just x          :-> nil
        Drain     -> perform mx <&> \x -> singletonList x :-> nil


-- | Apply a function to each item in the stream

map :: forall list a b action. List list =>
    (a -> b)
    -> Vendor (DrainableStream list a) (DrainableStream list b) action

map f = go
  where
    go = Vendor \case
        NextMaybe -> order NextMaybe <&> \case
            Nothing -> Nothing    :-> nil
            Just a  -> Just (f a) :-> go
        Drain -> order Drain <&> \xs ->
            fmap f xs :-> nil


-- | Collects everything from the stream

all :: forall list a action.
    Factory (DrainableStream list a) action (list a)

all = order Drain
