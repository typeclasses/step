module ActionList
  (
    {- * Type         -}  List (..),
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

newtype List m a =
  List
    { vendor :: Vendor (Const Void) (Unit (Maybe a)) m
    }

-- | @(xs '<>' ys) = ('append' xs ys)@
instance Semigroup (List m a)
  where
    (<>) = append

-- | @'mempty' = 'empty'@
instance Monoid (List m a)
  where
    mempty = empty

-- | @('fmap' f xs) = ('map' f xs)@
instance Functor (List m)
  where
    fmap f (List v) =
        List (v >-> Internal.map f)

instance Applicative (List m)
  where
    pure = List . Internal.singleton
    (<*>) = ap

-- | @(xs '>>=' f) = ('concatMap' f xs)@
instance Monad (List m)
  where
    xs >>= f = concatMap f xs

empty :: forall m a. List m a
empty = List Internal.nil

append :: forall m a.  List m a -> List m a -> List m a
append a b = List $
    Internal.append (vendor a) (vendor b)

map :: forall m a b. (a -> b) -> List m a -> List m b
map f xs = List (vendor xs >-> Internal.map f)

concatMap :: forall m a b. (a -> List m b) -> List m a -> List m b
concatMap f xs = List $
    vendor xs >-> Internal.concatMapVendor (vendor . f)

-- | Converts an ordinary list into an 'List'
fromList :: forall m a. [a] -> List m a
fromList = List . Internal.list

-- | A singleton list where the item is obtained by performing an action
perform :: forall m a. m a -> List m a
perform = List . Internal.actionSingleton

-- | Converts an 'List' into an ordinary list
toList :: forall a. List (Const Void) a -> [a]
toList xs = Job.eval $ vendor xs >- Internal.all

-- | Converts an 'List' into an action that returns all the items at once
run :: forall m a. Monad m => List m a -> m [a]
run xs = Job.run $ vendor xs >- Internal.all

next :: forall m a. Monad m => List m a -> m (Maybe (a, List m a))
next xs = fmap @m f $ Vendor.run (vendor xs) Unit
  where
    f :: Referral (Const Void) (Unit (Maybe a)) m (Maybe a) -> Maybe (a, List m a)
    f s = s & sequence & fmap @Maybe (\(Referral x v) -> (x, List v))

{-| Removes consecutive duplicate items, and yields each item along
    with the size of the repetition.

    For example:

    @'toList' ('group' ('fromList' \"Hrmm..."))@

    produces the result

    @[(1, \'H'), (1, \'r'), (2, \'m'), (3, \'.')]@
-}
group :: forall m a. Eq a => List m a -> List m (Natural, a)
group = List . (>-> Internal.group) . vendor

-- | The longest prefix matching the predicate
takeWhile :: forall m a. (a -> Bool) -> List m a -> List m a
takeWhile ok = List . (>-> Internal.while ok) . vendor
