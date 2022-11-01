module SupplyChain.Interface.ResettableTerminableStream
  (
    {- * Interface -} ResettableTerminableStream (..),
    {- * Vendors -} nil, singleton, list,
  )
  where

import SupplyChain

import SupplyChain.Interface.TerminableStream (IsTerminableStream (..))
import SupplyChain.Interface.Resettable (IsResettable (..))

import Control.Applicative (pure)
import Data.Kind (Type)
import Data.Maybe (Maybe (..))
import Data.Function (($), fix)

data ResettableTerminableStream i response =
    (response ~ Maybe i) => NextMaybe
  | (response ~ ()) => Reset

type ResettableTerminableStream :: Type -> Interface

instance IsTerminableStream item (ResettableTerminableStream item)
  where
    nextMaybe = NextMaybe

instance IsResettable (ResettableTerminableStream item)
  where
    reset = Reset

-- | The empty stream

nil :: forall up a action param.
    Vendor up (ResettableTerminableStream a) action param

nil = go
  where
    go :: Vendor up (ResettableTerminableStream a) action param
    go = Vendor \case
        NextMaybe -> pure $ Supply Nothing go
        Reset -> pure $ Supply () go

-- | Yields one item, then stops

singleton :: forall up a action param.
    a -> Vendor up (ResettableTerminableStream a) action param

singleton x = fix \r -> Vendor \case
    NextMaybe -> pure $ Supply (Just x) $ fix \go -> Vendor \case
        NextMaybe -> pure $ Supply Nothing go
        Reset -> pure $ Supply () r
    Reset -> pure $ Supply () r

-- | Yields each item from the list, then stops

list :: forall up a action param.
    [a] -> Vendor up (ResettableTerminableStream a) action param

list oxs = go oxs
  where
    go :: [a] -> Vendor up (ResettableTerminableStream a) action param
    go xs = fix \v -> Vendor \case
        Reset -> pure $ Supply () (go oxs)
        NextMaybe -> case xs of
            [] -> pure $ Supply Nothing v
            x : xs' -> pure $ Supply (Just x) (go xs')
