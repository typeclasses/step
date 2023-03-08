module Block.Class
  (
    Block,

    {- * Concatenation -} Concat (..), concatRefined, append, concatRecursively,

    {- * Dealing with single items -}
            first, last, terminal,
            Singleton (..), Pop (..), unpop, pushMaybe,

    {- * Operations involving numeric positions -}
            Positional (..), Take (..),

    {- * Single item at a numeric position -} Index (..),

    {- * Searching for items matching a predicate -}
            Search (..),
            spanPredicate, Span (..),
            findPredicate, Pivot (..),

    {- * Prefix detection -}
            biPrefix, BiPrefix (..),
            WhichOfTwo (First, Second),

    {- * Item equality -}
            ItemEquality (..),
            sameItemsTake, sameItemsPop, sameItemsPivot,
            sameItemsSpan, foldableEqOn, listEqOn,

    {- * Item equivalence -}
            ItemEquivalence (..), equality, itemEquality,

    {- * Nullability -}
            Refined (..),

    {- * Isomorphism with NonEmpty -}
            Construct (..), Enumerate (..),

    {- * End = Front | Back -} End (..), oppositeEnd,

    {- * Shortfall -} Shortfall (..),

    {- * State -} State (..), StateResult (..),
            runState, evalState, execState, stateless, get, put, modify,
  )
  where

import Essentials

import Block.State

import Data.List.NonEmpty (NonEmpty (..))
import Integer (Positive)
import Block.Shortfall (Shortfall (..))
import Prelude ((-))
import Data.Bool ((&&))
import Data.Function (on)
import Data.List.NonEmpty (nonEmpty)
import Data.Function (flip)
import Prelude (error)
import Data.Function (fix)
import Data.Ord (Ordering (..), compare)
import Block.End (End (..))
import GHC.Stack (HasCallStack)

import qualified Data.Maybe as Maybe
import qualified Data.Foldable as Foldable
import qualified Integer.Positive as Positive
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Semigroup as Semigroup
import qualified Block.End as End


---  Block  ---

class
  (
    Concat xs,
    Construct x xs,
    Enumerate x xs,
    Index x xs,
    ItemEquality xs,
    Positional xs,
    Search x xs,
    Singleton x xs
  ) =>
    Block x xs

instance (Eq x) => Block x (NonEmpty x)


---  Concat  ---

class Concat xs where
    (++) :: xs -> xs -> xs
    concat :: End -> NonEmpty xs -> xs

instance Concat (NonEmpty x) where

    (++) :: NonEmpty x -> NonEmpty x -> NonEmpty x
    (++) = (Semigroup.<>)

    concat :: End -> NonEmpty (NonEmpty x) -> NonEmpty x
    concat Front = Semigroup.sconcat
    concat Back = NonEmpty.reverse >>> Semigroup.sconcat

append :: Concat xs => End -> xs -> (xs -> xs)
append Front = (++)
append Back = flip (++)

{-| One possible implementation of 'concat', written in terms of '++' -}
concatRecursively :: Concat xs => End -> NonEmpty xs -> xs
concatRecursively e (x :| xs) = go x xs
  where
    go acc [] = acc
    go acc (y : ys) = go (append (oppositeEnd e) y acc) ys


---  Index  ---

class Index x xs | xs -> x where

    {-| Get the item at a particular position

    Returns 'Nothing' if the position is greater than 'length'.

    The first item's position is 1. -}
    at :: End -> Positive -> xs -> Maybe x

instance Index x (NonEmpty x) where

    at :: End -> Positive -> NonEmpty x -> Maybe x
    at Front n = case Positive.fromNatural (Positive.subtractOne n) of
        Nothing -> \(x :| _) -> Just x
        Just n' -> \(_ :| xs) -> go n' xs
      where
        go i xs = case xs of
            [] -> Nothing
            (x : xs') ->
                case Positive.fromNatural (Positive.subtractOne i) of
                    Nothing -> Just x
                    Just i' -> go i' xs'
    at Back n = NonEmpty.reverse >>> at Front n


---  Construct  ---

class Construct x xs | xs -> x where
    fromNonEmpty :: End -> NonEmpty x -> xs

instance Construct x (NonEmpty x) where

    fromNonEmpty :: End -> NonEmpty x -> NonEmpty x
    fromNonEmpty = \case Front -> id; Back -> NonEmpty.reverse


---   Enumerate  ---

class Enumerate x xs | xs -> x where

    toNonEmpty :: End -> xs -> NonEmpty x

    foldItems :: End -> (x -> a) -> (x -> State a ()) -> xs -> a

instance Enumerate x (NonEmpty x) where

    toNonEmpty :: End -> NonEmpty x -> NonEmpty x
    toNonEmpty = \case Front -> id; Back -> NonEmpty.reverse

    foldItems :: End -> (x -> a) -> (x -> State a ()) -> NonEmpty x -> a
    foldItems end initial step =
        toNonEmpty end >>> \(x0 :| xs) ->
        Foldable.foldl' (\s x -> step x & execState s) (initial x0) xs


---  Positional  ----

class Positional xs where

    {-| The number of items in the block -}
    length :: xs -> Positive

    {-| Separate a block into two parts by specifying the length
        of the first part

    When possible, the result is @('TakePart' taken remainder)@,
    where @taken@ is of the requested length and @remainder@ is
    everything else. If there is no remainder (when the requested
    number of items is exactly the block length), the result is
    'TakeAll'. If there are not enough items to take the requested
    number, the result is @('TakeInsufficient' s)@ where @s@ is a
    'Shortfall' indicating the difference between the block size
    and the requested number. -}
    take ::
        End -- ^ Which end to take from: 'Front' or 'Back'
        -> Positive -- ^ How many items to take
        -> xs -- ^ A block
        -> Take xs

{-| The result of 'take' -}
data Take xs =
    TakePart{ taken :: xs, takeRemainder :: xs }
      -- ^ The requested range covers part of the block
  | TakeAll
      -- ^ The requested range covers the entire block
  | TakeInsufficient Shortfall
      -- ^ The requested range exceeds the size of the block
  deriving stock (Eq, Ord, Show, Functor)

sameItemsTake :: ItemEquality xs => Take xs -> Take xs -> Bool
sameItemsTake = \case
    TakeAll -> \case TakeAll -> True; _ -> False
    TakeInsufficient s1 -> \case TakeInsufficient s2 -> s1 == s2; _ -> False
    TakePart a1 b1 -> \case TakePart a2 b2 -> sameItems a1 a2 && sameItems b1 b2; _ -> False

instance Positional (NonEmpty x) where

    length :: NonEmpty x -> Positive
    length = Positive.length

    take :: End -> Positive -> NonEmpty x -> Take (NonEmpty x)
    take Front 1 (_ :| []) = TakeAll
    take Front 1 (x :| y : z) = TakePart (x :| []) (y :| z)
    take Front n (_ :| []) = TakeInsufficient (Shortfall (n - 1))
    take Front n (x :| y : z) = take Front (n - 1) (y :| z) & \case
        TakeAll -> TakeAll
        TakePart a b -> TakePart (x :| NonEmpty.toList a) b
        t@TakeInsufficient{} -> t
    take Back n xs = take Front n (NonEmpty.reverse xs) & \case
        TakeAll -> TakeAll
        TakePart a b -> TakePart (NonEmpty.reverse a) (NonEmpty.reverse b)
        t@TakeInsufficient{} -> t


---  Search  ---

class Search x xs | xs -> x where

    {-| Separate a block into two parts by specifying a predicate
        for all the items in the first part

    Where possible, the result is @('SpanPart' spanned remainder)@,
    where @spanned@ is the longest contiguous series starting from the
    designated 'End' of items that match the predicate and @remainder@
    is everything else. If not even the first item matches, the result
    is 'SpanNone'. If all the items match, the result is 'SpanAll'. -}
    span ::
        End -- ^ Which end to start searching from: 'Front' or 'Back'
        -> (x -> State s Bool)
            -- ^ Keep taking as long as items match this predicate
        -> xs -- ^ A block
        -> State s (Span xs)

    {-| Search a block for the first item for which the given function
        returns @Just@

    If a match is found, the result is @(Just ('Pivot' a found b))@,
    which represents a division of the block into three parts. Part
    @(a :: Maybe xs)@ consists of the items that the search examined
    before finding a match. The @found@ value is the result obtained from
    the matching item. Part @(b :: Maybe xs)@ consists of the items that
    the search did not examine.

    If no match is found, the result is @Nothing@. -}
    find ::
        End  -- ^ Which end to start searching from: 'Front' or 'Back'
        -> (x -> State s (Maybe found))
            -- ^ Specification of what kind of item this search is looking
            --   for; when this function returns @Just@, an item is found
            --   and the search is over.
        -> xs -- ^ A block
        -> State s (Maybe (Pivot found xs))

{-| A successful result of 'Block.Class.find' -}
data Pivot a xs =
    Pivot
      { pivot :: a
      , split1 :: (xs, Maybe xs)
          -- ^ Split of the block where the first part includes the found item
      , split2 :: (Maybe xs, xs)
          -- ^ Split of the block where the second part includes the found item
      }
  deriving stock (Eq, Ord, Show, Functor)

{-| The result of 'Block.Class.span' -}
data Span xs =
    SpanPart
      -- ^ Some items were spanned by the predicate
      { spanned :: xs -- ^ The spanned items
      , spanRemainder :: xs -- ^ The remainder
      }
  | SpanNone
      -- ^ The first item does not satisfy the predicate
  | SpanAll
      -- ^ All items satisfy the predicate
  deriving stock (Eq, Ord, Show, Functor)

instance Search x (NonEmpty x) where

    span ::
        End -> (x -> State s Bool) -> NonEmpty x -> State s (Span (NonEmpty x))

    span Front f = fix \r (x :| xs) -> f x >>= \case
        False -> pure SpanNone
        True -> case xs of
            [] -> pure SpanAll
            y : ys -> r (y :| ys) <&> \case
                SpanAll        ->  SpanAll
                SpanNone       ->  SpanPart (x :| []) (y :| ys)
                SpanPart as b  ->  SpanPart (x :| NonEmpty.toList as) b

    span Back f = \xs -> span Front f (NonEmpty.reverse xs) <&> \case
        SpanNone      ->  SpanNone
        SpanAll       ->  SpanAll
        SpanPart a b  ->  SpanPart (NonEmpty.reverse a) (NonEmpty.reverse b)

    find :: End -> (x -> State s (Maybe found)) -> NonEmpty x
        -> State s (Maybe (Pivot found (NonEmpty x)))

    find Front f = fix \r (x :| xs) -> f x >>= \case
        Just pivot -> pure $ Just $ Pivot
          { pivot
          , split1 = (x :| [], nonEmpty xs)
          , split2 = (Nothing, x :| xs)
          }
        Nothing -> case xs of
            [] -> pure Nothing
            y : ys -> r (y :| ys) <&> fmap (pivotConsNonEmpty x)

    find Back f = \xs ->
        find Front f (NonEmpty.reverse xs) <&> fmap (fmap NonEmpty.reverse)

pivotConsNonEmpty :: x -> Pivot a (NonEmpty x) -> Pivot a (NonEmpty x)
pivotConsNonEmpty x Pivot{ pivot, split1 = (as, bsm), split2 = (asm, bs) } =
    Pivot{ pivot, split1 = (as', bsm), split2 = (asm', bs) }
  where
    as' = x :| Foldable.toList as
    asm' = asm & maybe (x :| []) (\as'' -> x :| Foldable.toList as'') & Just

findPredicate :: Search x xs => End -> (x -> Bool) -> xs -> Maybe (Pivot x xs)
findPredicate end f =
    stateless . find end (\x -> pure $ if f x then Just x else Nothing)

spanPredicate :: Search x xs => End -> (x -> Bool) -> xs -> Span xs
spanPredicate end f = stateless . span end (pure . f)

sameItemsPivot :: Eq x => ItemEquality xs => Pivot x xs -> Pivot x xs -> Bool
sameItemsPivot (Pivot x1 (as1, _) (_, bs1)) (Pivot x2 (as2, _) (_, bs2)) =
    x1 == x2 && sameItems as1 as2 && sameItems bs1 bs2

sameItemsSpan :: ItemEquality xs => Span xs -> Span xs -> Bool
sameItemsSpan = \case
    SpanNone -> \case SpanNone -> True; _ -> False
    SpanAll -> \case SpanAll -> True; _ -> False
    SpanPart a1 b1 -> \case
        SpanPart a2 b2 -> sameItems a1 a2 && sameItems b1 b2
        _ -> False


---  Singleton  ---

class Singleton x xs | xs -> x where

    {-| A block with a single item -}
    singleton ::
        x -- ^ An item
        -> xs -- ^ A block consisting of exactly one item

    {-| Cut a block into two parts: The item from the front/back,
        and maybe a remainder -}
    pop ::
        End -- ^ 'Front' or 'Back'
        -> xs -- ^ The block
        -> Pop x xs -- ^ Division of the block into item and remainder

    {-| Add one item onto the front/back of a block -}
    push ::
        End -- ^ 'Front' or 'Back'
        -> x -- ^ An item
        -> xs -- ^ A block
        -> xs -- ^ A new block with the item appended to it

data Pop x xs = Pop{ item :: x, popRemainder :: Maybe xs }
    deriving stock (Eq, Ord, Show)

instance Singleton x (NonEmpty x) where

    singleton :: x -> NonEmpty x
    singleton = (:| [])

    pop :: End -> NonEmpty x -> Pop x (NonEmpty x)
    pop = \case
        Front -> \(x :| xs) -> Pop x (nonEmpty xs)
        Back -> \xs ->
            let p = pop Front (NonEmpty.reverse xs)
            in p{ popRemainder = NonEmpty.reverse <$> popRemainder p }

    push :: End -> x -> NonEmpty x -> NonEmpty x
    push = \case
        Front -> \x (y :| ys) -> x :| y : ys
        Back -> \x xs -> NonEmpty.reverse $
            push Front x (NonEmpty.reverse xs)

{-| The inverse of 'pop' -}
unpop :: (Singleton x xs) =>
    End -- ^ 'Front' or 'Back'
    -> Pop x xs -- ^ Division of a block into item and remainder
    -> xs -- ^ Item and remainder concatenated back together
unpop s (Pop x xm) = case xm of
    Nothing -> singleton x
    Just xs -> push s x xs

terminal :: (Singleton x xs) => End -> xs -> x
terminal e = item . pop e

{-| The item at the 'Front' of a block -}
first :: (Singleton x xs) =>
    xs -- ^ A block
    -> x -- ^ The block's first item
first = terminal Front

{-| The item at the 'Back' of a block -}
last :: (Singleton x xs) =>
    xs -- ^ A block
    -> x -- ^ The block's first item
last = terminal Back

pushMaybe :: (Singleton x xs) => End -> Maybe x -> Maybe xs -> Maybe xs
pushMaybe end (Just x) (Just xs)  =  Just (push end x xs)
pushMaybe _   Nothing  (Just xs)  =  Just xs
pushMaybe _   (Just x) Nothing    =  Just (singleton x)
pushMaybe _   Nothing  Nothing    =  Nothing

sameItemsPop :: Eq x => ItemEquality xs => Pop x xs -> Pop x xs -> Bool
sameItemsPop (Pop x1 xs1) (Pop x2 xs2) = x1 == x2 && foldableEqOn sameItems xs1 xs2


---  Same items  ---

class ItemEquality xs where
    sameItems :: xs -> xs -> Bool

instance (Eq x) => ItemEquality (NonEmpty x) where

    sameItems :: NonEmpty x -> NonEmpty x -> Bool
    sameItems = (==)

foldableEqOn :: Foldable f => (x -> x -> Bool) -> f x -> f x -> Bool
foldableEqOn f = listEqOn f `on` Foldable.toList

listEqOn :: (x -> x -> Bool) -> [x] -> [x] -> Bool
listEqOn _ [] [] = True
listEqOn _ [] (_ : _) = False
listEqOn _ (_ : _) [] = False
listEqOn f (a : as) (b : bs) = f a b && listEqOn f as bs


---  Item equivalence  ---

{-| An equivalence on @('Block.Class.Item' xs)@, expressed as an equivalence
on @xs@ that must satisfy the following condition:

There must exist a predicate

@eq :: (Item xs, Item xs) -> Bool@

such that

@'equivalentItems' (a, b)@

is the same as

@
('Block.Class.length' a == 'Block.Class.length' b)
    && ('Data.Foldable.all' eq ('Data.NonEmpty.zip' ('Block.Class.toNonEmpty' a) ('Block.Class.toNonEmpty' b)))
@

The point here is that we want to work with efficiently packed string
types like @(Text1)@ but still be able to reason about them as if they
were @(NonEmpty Char])@. -}
newtype ItemEquivalence xs =
    ItemEquivalence{ equivalentItems :: (xs, xs) -> Bool }

{-| Equivalence based on 'Eq' -}
equality :: Eq xs => ItemEquivalence xs
equality = ItemEquivalence \(a, b) -> a == b

itemEquality :: ItemEquality xs => ItemEquivalence xs
itemEquality = ItemEquivalence \(a, b) -> sameItems a b


---  Refined  ---

class Refined nul xs | xs -> nul where

    refine :: nul -> Maybe xs

    generalize :: xs -> nul

    {-| Defined only where 'refine' produces 'Just' -}
    assume :: HasCallStack => nul -> xs
    assume = refine >>> Maybe.fromJust

    {-# minimal refine, generalize #-}

instance Refined [x] (NonEmpty x) where

    refine :: [x] -> Maybe (NonEmpty x)
    refine = nonEmpty

    generalize :: NonEmpty x -> [x]
    generalize = Foldable.toList

    assume :: [x] -> NonEmpty x
    assume (x : xs) = x :| xs
    assume [] = error "Block.assume NonEmpty"

concatRefined :: (Monoid nul, Refined nul xs) => [xs] -> nul
concatRefined = fmap generalize >>> Foldable.fold


---  Bi-prefix  ---

{-| Given a pair of blocks, determine whether either is a prefix
    of the other, according to an item equivalence

If the first block is a prefix of the second, the result is
@('IsPrefix' 'First' a b)@. If the second block is a prefix of
the first, the result is @('IsPrefix' 'Second' a b)@. The fields
@(a)@ and @(b)@ constitute a partition of the larger block, where
@(a)@ is the portion equivalent to the shorter block and @(b)@
is the remainder.

If both blocks are identical (the first is a prefix of the second
and vice versa), the result is 'BothPrefix'. If neither block is a
prefix of the other, the result is 'NoPrefixRelation'. -}
biPrefix :: Positional xs =>
    ItemEquivalence xs -- ^ How to determine equivalence of two
                       --   sub-blocks (such as 'Block.Class.equality')
    -> End
    -> (xs, xs) -- ^ Two blocks to compare
    -> BiPrefix xs
biPrefix (equivalentItems -> same) end pair =
    case whichIsShorter pair of
        Nothing -> if same pair then BothPrefix else NoPrefixRelation
        Just theShorter -> case take end (length short) long of
            TakePart prefix suffix | same (prefix, short) ->
                IsPrefix theShorter prefix suffix
            _ -> NoPrefixRelation
          where
            (short, long) = case theShorter of
                First -> pair
                Second -> swap pair

  where
    swap :: (a, b) -> (b, a)
    swap (a, b) = (b, a)

    whichIsShorter :: Positional xs => (xs, xs) -> Maybe WhichOfTwo
    whichIsShorter (a, b) = case (compare `on` length) a b of
        EQ -> Nothing
        LT -> Just First
        GT -> Just Second

data WhichOfTwo = First | Second
    deriving stock (Eq, Ord, Show, Enum, Bounded)

data BiPrefix xs =
    BothPrefix -- ^ The two blocks are equivalent
  | NoPrefixRelation -- ^ Neither block is a prefix of the other
  | IsPrefix
      -- ^ The ('First' or 'Second') block is a prefix of the other
      WhichOfTwo xs xs
    deriving stock (Eq, Ord, Show, Functor)


---  End  ---

oppositeEnd :: End -> End
oppositeEnd = End.opposite
