module SupplyChain.TerminableStream where

import SupplyChain

import Control.Applicative (pure)
import Control.Monad ((>>=))
import Data.Bool (Bool (..))
import Data.Function (($), id)
import Data.Functor (Functor)
import Data.Kind (Type)
import Data.Maybe (Maybe (..))


data TerminableStream item response =
    (response ~ Maybe item) => NextMaybe
        -- ^ The next item, or 'Nothing' if input is exhausted

type TerminableStream :: Type -> Interface


-- | Yields each item from the list, then stops

list :: forall up a action. Functor action =>
    [a] -> Vendor up (TerminableStream a) action

list = go
  where
    go :: [a] -> Vendor up (TerminableStream a) action
    go = \case
        []      ->  nil
        x : xs  ->  Vendor \NextMaybe -> pure $ Just x :-> go xs


-- | The empty stream

nil :: forall up a action. Functor action =>
    Vendor up (TerminableStream a) action

nil = go
  where
    go :: Vendor up (TerminableStream a) action
    go = Vendor \NextMaybe -> pure $ Nothing :-> go


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


{-| Applies the function to each result obtained from upstream,
    and yields each result from the list to the downstream
-}

concatMap :: forall a b action. Functor action =>
    (a -> [b]) -> Vendor (TerminableStream a) (TerminableStream b) action

concatMap f = go []
  where
    go :: [b] -> Vendor (TerminableStream a) (TerminableStream b) action
    go bs = Vendor \NextMaybe -> case bs of
        b : bs' -> pure $ Just b :-> go bs'
        [] -> order NextMaybe >>= \case
            Nothing -> pure $ Nothing :-> nil
            Just a -> offer (go (f a)) NextMaybe


-- | Flattens a stream of lists

concat :: forall a action. Functor action =>
    Vendor (TerminableStream [a]) (TerminableStream a) action

concat = concatMap id
