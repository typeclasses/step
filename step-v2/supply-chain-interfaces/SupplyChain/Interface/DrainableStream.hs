{-| Like 'TerminableStream', but with an additional operation
    for taking the entire remainder -}
module SupplyChain.Interface.DrainableStream where

import Essentials
import SupplyChain

import Next.Interface (TerminableStream (..), Next (..), Step (..))

import qualified Data.List as List


class Functor list => List list where
    emptyList :: list a
    singletonList :: a -> list a
    unconsList :: list a -> Maybe (a, list a)


instance List [] where
    emptyList = []
    singletonList = (: [])
    unconsList = List.uncons


data DrainableStream list item response =
    (response ~ Step item) => NextMaybe
        -- ^ The next item, or 'Nothing' if input is exhausted
        --
        -- It is assumed that after a 'Nothing' response is given,
        -- all subsequent responses will also be 'Nothing'.
  | (response ~ list item) => Drain


instance TerminableStream item (DrainableStream list item) where
    liftNext Next = NextMaybe


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
            NextMaybe  ->  pure $ Referral (Item x) (go xs')
            Drain      ->  pure $ Referral xs nil


-- | The empty stream

nil :: forall up list a action. List list =>
    Vendor up (DrainableStream list a) action

nil = go
  where
    go :: Vendor up (DrainableStream list a) action
    go = Vendor \case
        NextMaybe  ->  pure $ Referral End       go
        Drain      ->  pure $ Referral emptyList go


-- | Yields one item, then stops

singleton :: forall up list a action. List list =>
    a -> Vendor up (DrainableStream list a) action

singleton x = Vendor \case
    NextMaybe  ->  pure $ Referral (Item x)          nil
    Drain      ->  pure $ Referral (singletonList x) nil


-- | Performs one action, yields the resulting item, then stops

actionSingleton :: forall up list a action. List list =>
    action a -> Vendor up (DrainableStream list a) action

actionSingleton mx =
    Vendor \case
        NextMaybe  ->  perform mx <&> \x -> Referral (Item x)          nil
        Drain      ->  perform mx <&> \x -> Referral (singletonList x) nil


-- | Apply a function to each item in the stream

map :: forall list a b action. List list =>
    (a -> b)
    -> Vendor (DrainableStream list a) (DrainableStream list b) action

map f = go
  where
    go = Vendor \case
        NextMaybe -> order NextMaybe <&> \case
            End    ->  Referral End          nil
            Item a ->  Referral (Item (f a)) go
        Drain -> order Drain <&> \xs ->
            Referral (fmap f xs) nil


-- | Collects everything from the stream

all :: forall list a action.
    Job (DrainableStream list a) action (list a)

all = order Drain
