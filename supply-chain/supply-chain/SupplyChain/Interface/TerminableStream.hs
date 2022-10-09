{- |

Description: The 'TerminableStream' interface, for consuming lists one item at a time

-}

module SupplyChain.Interface.TerminableStream
  (
    {- * Interface -} TerminableStream (..),
    {- * Vendors -} nil, singleton, actionSingleton, list, map, concatMap, concat, while, group,
    {- * Vendor composition -} append, concatMapVendor,
    {- * Factories -} all,
  )
  where

import SupplyChain hiding (map)

import Control.Applicative (pure)
import Control.Monad ((>>=))
import Data.Bool (Bool (..))
import Data.Eq (Eq, (==))
import Data.Function (($), id)
import Data.Functor ((<$>), (<&>))
import Data.Kind (Type)
import Data.Maybe (Maybe (..))
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

list :: forall up a action.
    [a] -> Vendor up (TerminableStream a) action

list = go
  where
    go :: [a] -> Vendor up (TerminableStream a) action
    go = \case
        []      ->  nil
        x : xs  ->  Vendor \NextMaybe -> pure (Just x :-> go xs)


-- | The empty stream

nil :: forall up a action.
    Vendor up (TerminableStream a) action

nil = go
  where
    go :: Vendor up (TerminableStream a) action
    go = Vendor \NextMaybe -> pure (Nothing :-> go)


-- | Yields one item, then stops

singleton :: forall up a action.
    a -> Vendor up (TerminableStream a) action

singleton x = Vendor \NextMaybe -> pure (Just x :-> nil)


-- | Performs one action, yields the resulting item, then stops

actionSingleton :: forall up a action.
    action a -> Vendor up (TerminableStream a) action

actionSingleton mx =
    Vendor \NextMaybe -> perform mx <&> \x -> Just x :-> nil


-- | Apply a function to each item in the stream

map :: forall a b action.
    (a -> b) -> Vendor (TerminableStream a) (TerminableStream b) action

map f = Vendor \NextMaybe -> order NextMaybe <&> \case
    Nothing -> Nothing    :-> nil
    Just a  -> Just (f a) :-> map f


{-| Applies the function to each result obtained from upstream,
    and yields each result from the list to the downstream
-}

concatMap :: forall a b action.
    (a -> [b]) -> Vendor (TerminableStream a) (TerminableStream b) action

concatMap f = Vendor \NextMaybe -> go []
  where
    go :: [b] -> Factory (TerminableStream a) action
        (Supply (TerminableStream a) (TerminableStream b) action (Maybe b))
    go = \case
        b : bs' -> pure (Just b :-> Vendor \NextMaybe -> go bs')
        [] -> order NextMaybe >>= \case
            Nothing -> pure (Nothing :-> nil)
            Just a  -> go (f a)


-- | Like 'concatMap', but the function gives a vendor instead of a list

concatMapVendor :: forall a b action.
    (a -> Vendor NoInterface (TerminableStream b) action)
    -> Vendor (TerminableStream a) (TerminableStream b) action

concatMapVendor f = go
  where
    go = Vendor \NextMaybe -> order NextMaybe >>= \case
        Nothing -> pure $ Nothing :-> nil
        Just x -> offer (append (noVendor >-> f x) go) NextMaybe


-- | Flattens a stream of lists

concat :: forall a action.
    Vendor (TerminableStream [a]) (TerminableStream a) action

concat = concatMap id


-- | Yields the longest stream prefix matching the predicate

while :: forall a action.
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

append :: forall up a action.
       Vendor up (TerminableStream a) action
    -> Vendor up (TerminableStream a) action
    -> Vendor up (TerminableStream a) action

append a b = Vendor \NextMaybe ->
    offer a NextMaybe >>= \case
        Nothing :-> _ -> offer b NextMaybe
        Just x :-> a' -> pure $ Just x :-> append a' b


-- | Collects everything from the stream

all :: forall a action.
    Factory (TerminableStream a) action [a]

all = go
  where
    go = order NextMaybe >>= \case
        Nothing -> pure []
        Just x -> (x :) <$> go


{-| Removes consecutive duplicate items, and yields each item along
    with the number of duplicates that were removed.

    For example:

    @
    'eval' ('list' "Hrmm..." '>->' 'group' '>->' 'all')
    @

    produces the result

    @
    [(0, \'H'), (0, \'r'), (1, \'m'), (2, \'.')]
    @
-}

group :: forall a action. Eq a =>
    Vendor (TerminableStream a) (TerminableStream (Natural, a)) action

group = Vendor \NextMaybe ->
    order NextMaybe >>= \case
        Nothing -> pure $ Nothing :-> nil
        Just x -> start x
  where
    start :: a -> Factory (TerminableStream a) action
        ( Supply (TerminableStream a) (TerminableStream (Natural, a))
          action (Maybe (Natural, a))
        )
    start = go 0

    go n x = order NextMaybe >>= \case
        Nothing -> pure $ Just (n, x) :-> nil
        Just y ->
          if y == x
          then go (n + 1) x
          else pure $ Just (n, x) :-> Vendor \NextMaybe -> start y
