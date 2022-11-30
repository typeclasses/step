module ActionList
  (
    {- * Type         -}  List (..),
    {- * Introduction -}  fromList, perform,
    {- * Elimination  -}  toList, run,
    {- * Single step  -}  next,
    {- * Modification -}  takeWhile, group,
    {- * Combination  -}  append, concatMap,
  )
  where

import qualified Internal

import SupplyChain (Vendor (Vendor, handle), Referral (Referral), Unit (Unit), (>->), (>-), Job)
import qualified SupplyChain.Vendor as Vendor
import qualified SupplyChain.Job as Job

import Control.Applicative (Applicative, pure, (<*>))
import Control.Monad (Monad, (>>=), ap)
import Data.Bool (Bool)
import Data.Eq (Eq, (==))
import Data.Function (($), (&))
import Data.Functor (Functor, fmap, (<&>))
import Data.Functor.Const (Const)
import Data.Maybe (Maybe (Just, Nothing))
import Data.Monoid (Monoid, mempty)
import Data.Semigroup (Semigroup, (<>))
import Data.Traversable (sequence)
import Data.Void (Void)
import Numeric.Natural (Natural)
import Prelude ((+))

newtype List m a =
  List
    { vendor :: forall up. Vendor up (Unit (Maybe a)) m
    }

-- | @(xs '<>' ys) = ('append' xs ys)@
instance Semigroup (List m a)
  where
    (<>) = append

-- | @'mempty' = 'empty'@
instance Monoid (List m a)
  where
    mempty = List go
      where
        go :: Vendor up (Unit (Maybe a)) action
        go = Vendor \Unit -> pure $ Referral Nothing go

-- | @('fmap' f xs) = ('map' f xs)@
instance Functor (List m)
  where
    fmap f xs = List (vendor xs >-> go)
      where
        go = Vendor \Unit -> Job.order Unit <&> \xm -> case xm of
            Nothing -> Referral Nothing (vendor mempty)
            Just a -> Referral (Just (f a)) go

instance Applicative (List m)
  where
    pure x = List $ Vendor \Unit -> pure $ Referral (Just x) (vendor mempty)
    (<*>) = ap

-- | @(xs '>>=' f) = ('concatMap' f xs)@
instance Monad (List m)
  where
    xs >>= f = concatMap f xs

append :: forall m a.  List m a -> List m a -> List m a
append = \a b -> List $ append' (vendor a) (vendor b)
  where
    append' :: forall up.
         Vendor up (Unit (Maybe a)) m
      -> Vendor up (Unit (Maybe a)) m
      -> Vendor up (Unit (Maybe a)) m
    append' a b = Vendor \Unit -> do
        xm <- handle a Unit
        case xm of
            Referral Nothing _ -> handle b Unit
            Referral (Just x) a' -> pure $ Referral (Just x) (append' a' b)

concatMap :: forall m a b. (a -> List m b) -> List m a -> List m b
concatMap f xs = List $
    vendor xs >-> Internal.concatMapVendor (\x -> vendor (f x))

-- | Converts an ordinary list into an 'List'
fromList :: forall m a. [a] -> List m a
fromList a = List (go a)
  where
    go :: [a] -> Vendor up (Unit (Maybe a)) action
    go [] = vendor mempty
    go (x : xs) = Vendor \Unit -> pure $ Referral (Just x) (go xs)

-- | A singleton list where the item is obtained by performing an action
perform :: forall m a. m a -> List m a
perform mx = List $ Vendor \Unit -> Job.perform mx <&> \x ->
    Referral (Just x) (vendor mempty)

-- | Converts an 'List' into an ordinary list
toList :: forall a. List (Const Void) a -> [a]
toList xs = Job.eval $ vendor xs >- Internal.all

-- | Converts an 'List' into an action that returns all the items at once
run :: forall m a. Monad m => List m a -> m [a]
run xs = Job.run $ vendor xs >- Internal.all

next :: forall m a. Monad m => List m a -> m (Maybe (a, List m a))
next xs = Vendor.run (vendor xs) Unit <&> \s ->
    sequence s & fmap @Maybe (\(Referral x v') -> (x, List (Vendor.absurd >-> v')))

{-| Removes consecutive duplicate items, and yields each item along
    with the size of the repetition.

    For example:

    @'toList' ('group' ('fromList' \"Hrmm..."))@

    produces the result

    @[(1, \'H'), (1, \'r'), (2, \'m'), (3, \'.')]@
-}
group :: forall m a. Eq a => List m a -> List m (Natural, a)
group a = List $ vendor a >-> Vendor \Unit -> do
    xm <- Job.order Unit
    case xm of
        Nothing -> pure $ Referral Nothing nil
        Just x -> start x
  where
    start = go 1
    go (n :: Natural) (x :: a) = do
        ym <- Job.order Unit :: Job (Unit (Maybe a)) m (Maybe a)
        case ym of
            Nothing -> yield nil
            Just y | y == x -> go (n + 1) x
            Just y -> yield $ Vendor \Unit -> start y
      where
        yield v = pure $ Referral (Just (n, x)) v

    nil = vendor mempty :: Vendor (Unit (Maybe a)) (Unit (Maybe (Natural, a))) m

-- | The longest prefix matching the predicate
takeWhile :: forall m a. (a -> Bool) -> List m a -> List m a
takeWhile ok x = List (vendor x >-> Internal.while ok)
