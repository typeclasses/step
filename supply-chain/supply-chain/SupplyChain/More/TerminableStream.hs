module SupplyChain.More.TerminableStream where

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


list :: forall up a action. Functor action =>
    [a] -> Vendor up (TerminableStream a) action

list = go
  where
    go :: [a] -> Vendor up (TerminableStream a) action
    go = \case
        []      ->  endOfStream
        x : xs  ->  Vendor \NextMaybe -> pure $ Just x :-> go xs


endOfStream :: forall up a action. Functor action =>
    Vendor up (TerminableStream a) action

endOfStream = go
  where
    go :: Vendor up (TerminableStream a) action
    go = Vendor \NextMaybe -> pure $ Nothing :-> go


terminableWhile :: forall a action. Functor action =>
    (a -> Bool)
    -> Vendor (TerminableStream a) (TerminableStream a) action

terminableWhile ok = v
  where
    v = Vendor \NextMaybe ->
        order NextMaybe >>= \case
            Just x | ok x  ->  pure $ Just x  :-> v
            _              ->  pure $ Nothing :-> endOfStream


terminableConcatMap :: forall a b action. Functor action =>
    (a -> [b]) -> Vendor (TerminableStream a) (TerminableStream b) action

terminableConcatMap f = go []
  where
    go :: [b] -> Vendor (TerminableStream a) (TerminableStream b) action
    go bs = Vendor \NextMaybe -> case bs of
        b : bs' -> pure $ Just b :-> go bs'
        [] -> order NextMaybe >>= \case
            Nothing -> pure $ Nothing :-> endOfStream
            Just a -> offer (go (f a)) NextMaybe


terminableConcat :: forall a action. Functor action =>
    Vendor (TerminableStream [a]) (TerminableStream a) action

terminableConcat = terminableConcatMap id
