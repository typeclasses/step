module ActionList
  (
    {- * Type         -}  ActionList (..),
    {- * Introduction -}  fromList, perform,
    {- * Elimination  -}  toList, run,
    {- * Single step  -}  next,
    {- * Modification -}  map, takeWhile, group,
    {- * Combination  -}  append, concatMap,
  )
  where

import qualified Internal

import SupplyChain (Vendor, Referral (Referral), Unit (Unit), (>->), (>-))
import qualified SupplyChain.Vendor as Vendor
import qualified SupplyChain.Job as Job

import Control.Applicative (Applicative, pure, (<*>))
import Control.Monad (Monad, (>>=), ap)
import Data.Bool (Bool)
import Data.Eq (Eq)
import Data.Function ((.), ($), (&))
import Data.Functor (Functor, fmap)
import Data.Functor.Const (Const)
import Data.Maybe (Maybe)
import Data.Monoid (Monoid, mempty)
import Data.Semigroup (Semigroup, (<>))
import Data.Traversable (sequence)
import Data.Void (Void)
import Numeric.Natural (Natural)

newtype ActionList m a =
  VendorActionList
    { actionListVendor :: Vendor (Const Void) (Unit (Maybe a)) m
    }

-- | @(xs '<>' ys) = ('append' xs ys)@
instance Semigroup (ActionList m a)
  where
    (<>) = append

-- | @'mempty' = 'empty'@
instance Monoid (ActionList m a)
  where
    mempty = empty

-- | @('fmap' f xs) = ('map' f xs)@
instance Functor (ActionList m)
  where
    fmap f (VendorActionList v) =
        VendorActionList (v >-> Internal.map f)

instance Applicative (ActionList m)
  where
    pure = VendorActionList . Internal.singleton
    (<*>) = ap

-- | @(xs '>>=' f) = ('concatMap' f xs)@
instance Monad (ActionList m)
  where
    xs >>= f = concatMap f xs

empty :: forall m a. ActionList m a
empty = VendorActionList Internal.nil

append :: forall m a.  ActionList m a -> ActionList m a -> ActionList m a
append a b = VendorActionList $
    Internal.append (actionListVendor a) (actionListVendor b)

map :: forall m a b. (a -> b) -> ActionList m a -> ActionList m b
map f xs = VendorActionList (actionListVendor xs >-> Internal.map f)

concatMap :: forall m a b. (a -> ActionList m b) -> ActionList m a -> ActionList m b
concatMap f xs = VendorActionList $
    actionListVendor xs >-> Internal.concatMapVendor (actionListVendor . f)

-- | Converts an ordinary list into an 'ActionList'
fromList :: forall m a. [a] -> ActionList m a
fromList = VendorActionList . Internal.list

-- | A singleton list where the item is obtained by performing an action
perform :: forall m a. m a -> ActionList m a
perform = VendorActionList . Internal.actionSingleton

-- | Converts an 'ActionList' into an ordinary list
toList :: forall a. ActionList (Const Void) a -> [a]
toList xs = Job.eval $ actionListVendor xs >- Internal.all

-- | Converts an 'ActionList' into an action that returns all the items at once
run :: forall m a. Monad m => ActionList m a -> m [a]
run xs = Job.run $ actionListVendor xs >- Internal.all

next :: forall m a. Monad m => ActionList m a -> m (Maybe (a, ActionList m a))
next xs = fmap @m f $ Vendor.run (actionListVendor xs) Unit
  where
    f :: Referral (Const Void) (Unit (Maybe a)) m (Maybe a) -> Maybe (a, ActionList m a)
    f s = s & sequence & fmap @Maybe (\(Referral x v) -> (x, VendorActionList v))

{-| Removes consecutive duplicate items, and yields each item along
    with the size of the repetition.

    For example:

    @'toList' ('group' ('fromList' \"Hrmm..."))@

    produces the result

    @[(1, \'H'), (1, \'r'), (2, \'m'), (3, \'.')]@
-}
group :: forall m a. Eq a => ActionList m a -> ActionList m (Natural, a)
group = VendorActionList . (>-> Internal.group) . actionListVendor

-- | The longest prefix matching the predicate
takeWhile :: forall m a. (a -> Bool) -> ActionList m a -> ActionList m a
takeWhile ok = VendorActionList . (>-> Internal.while ok) . actionListVendor
