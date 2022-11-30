module Internal
  (
    {- * Vendors -} nil, singleton, actionSingleton, action,
        map, concatMap, concat, while,
    {- * Vendor composition -} append, concatMapVendor,
    {- * Jobs -} all,
  )
  where

import SupplyChain
import qualified SupplyChain.Alter as Alter

import Control.Applicative (pure)
import Control.Monad ((>>=))
import Data.Bool (Bool (..))
import Data.Function (($), id, (&))
import Data.Functor ((<$>), (<&>))
import Data.Maybe (Maybe (..))
import Data.Functor.Const (Const)
import Data.Void (Void)

action :: forall up a action. action (Maybe a) -> Vendor up (Unit (Maybe a)) action
action a = go
  where
    go :: Vendor up (Unit (Maybe a)) action
    go = Vendor{ handle = \Unit -> perform a <&> (`Referral` go) }

-- | The empty stream
nil :: forall up a action. Vendor up (Unit (Maybe a)) action
nil = go
  where
    go :: Vendor up (Unit (Maybe a)) action
    go = Vendor{ handle = \Unit -> pure $ Referral Nothing go }

-- | Yields one item, then stops
singleton :: forall up a action. a -> Vendor up (Unit (Maybe a)) action
singleton x = Vendor{ handle = \Unit -> pure $ Referral (Just x) nil }

-- | Performs one action, yields the resulting item, then stops
actionSingleton :: forall up a action. action a -> Vendor up (Unit (Maybe a)) action
actionSingleton mx = Vendor{ handle = \Unit -> perform mx <&> \x -> Referral (Just x) nil }

-- | Apply a function to each item in the stream
map :: forall a b action. (a -> b) -> Vendor (Unit (Maybe a)) (Unit (Maybe b)) action
map f = go
  where
    go = Vendor
      { handle = \Unit -> order Unit <&> \xm -> case xm of
          Nothing -> Referral Nothing nil
          Just a -> Referral (Just (f a)) go
      }

{-| Applies the function to each result obtained from upstream,
    and yields each result from the list to the downstream -}
concatMap :: forall a b action. (a -> [b])
   -> Vendor (Unit (Maybe a)) (Unit (Maybe b)) action
concatMap f = Vendor{ handle = \Unit -> go [] }
  where
    go :: [b] -> Job (Unit (Maybe a)) action
        (Referral (Unit (Maybe a)) (Unit (Maybe b)) action (Maybe b))
    go bs = case bs of
        b : bs' -> pure $ Referral (Just b) Vendor{ handle = \Unit -> go bs' }
        [] -> order Unit >>= \xm -> case xm of
            Nothing -> pure $ Referral Nothing nil
            Just a  -> go (f a)

-- | Like 'concatMap', but the function gives a vendor instead of a list
concatMapVendor :: forall a b action.
    (a -> Vendor (Const Void) (Unit (Maybe b)) action)
    -> Vendor (Unit (Maybe a)) (Unit (Maybe b)) action
concatMapVendor f = go
  where
    go = Vendor
        { handle = \Unit -> order Unit >>= \xm -> case xm of
            Nothing -> pure $ Referral Nothing nil
            Just x -> handle (append v go) Unit
              where
                v = f x & Alter.vendor' (Alter.request' $ \z -> case z of {})
        }

-- | Flattens a stream of lists
concat :: forall a action. Vendor (Unit (Maybe [a])) (Unit (Maybe a)) action
concat = concatMap id

-- | Yields the longest stream prefix matching the predicate
while :: forall a action. (a -> Bool) -> Vendor (Unit (Maybe a)) (Unit (Maybe a)) action
while ok = v
  where
    v = Vendor
        { handle = \Unit ->
            order Unit >>= \xm -> case xm of
                Just x | ok x  ->  pure $ Referral (Just x) v
                _              ->  pure $ Referral Nothing nil
        }

-- | Yields all the items of the first stream, followed by all the items of the second
append :: forall up a action.
       Vendor up (Unit (Maybe a)) action
    -> Vendor up (Unit (Maybe a)) action
    -> Vendor up (Unit (Maybe a)) action
append a b = Vendor
    { handle = \Unit ->
        handle a Unit >>= \xm -> case xm of
            Referral Nothing _ -> handle b Unit
            Referral (Just x) a' -> pure $ Referral (Just x) (append a' b)
    }

-- | Collects everything from the stream
all :: forall a action. Job (Unit (Maybe a)) action [a]
all = go
  where
    go = order Unit >>= \xm -> case xm of
        Nothing -> pure []
        Just x -> (x :) <$> go
