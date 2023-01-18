module SupplyChain.Interface.ResettableTerminableStream
  (
    {- * Interface -} ResettableTerminableStream (..),
    {- * Vendors -} nil, singleton, list,
  )
  where

import Essentials
import SupplyChain

import Next.Interface (TerminableStream (..), Next (..), Step (..))
import SupplyChain.Interface.Resettable (IsResettable (..))

import Data.Function (fix)

data ResettableTerminableStream i response =
    (response ~ Step i) => NextMaybe
  | (response ~ ()) => Reset

instance TerminableStream item (ResettableTerminableStream item) where
    liftNext Next = NextMaybe

instance IsResettable (ResettableTerminableStream item) where
    reset = Reset

-- | The empty stream

nil :: forall up a action.
    Vendor up (ResettableTerminableStream a) action

nil = go
  where
    go :: Vendor up (ResettableTerminableStream a) action
    go = Vendor \case
        NextMaybe -> pure $ Referral End go
        Reset -> pure $ Referral () go

-- | Yields one item, then stops

singleton :: forall up a action.
    a -> Vendor up (ResettableTerminableStream a) action

singleton x = fix \r -> Vendor \case
    NextMaybe -> pure $ Referral (Item x) $ fix \go -> Vendor \case
        NextMaybe -> pure $ Referral End go
        Reset -> pure $ Referral () r
    Reset -> pure $ Referral () r

-- | Yields each item from the list, then stops

list :: forall up a action.
    [a] -> Vendor up (ResettableTerminableStream a) action

list oxs = go oxs
  where
    go :: [a] -> Vendor up (ResettableTerminableStream a) action
    go xs = fix \v -> Vendor \case
        Reset -> pure $ Referral () (go oxs)
        NextMaybe -> case xs of
            [] -> pure $ Referral End v
            x : xs' -> pure $ Referral (Item x) (go xs')
