{-# language FlexibleInstances, Trustworthy #-}

module Step.Input.Buffer
  (
    Buffer, singleton, isEmpty, empty, toListT, fold, headChar, unconsChar, unconsChunk,

    -- * State operations
    considerChunk, takeChar, takeChunk, takeString, takeNontrivialString, TakeStringResult (..), advance,
  )
  where

import Step.Internal.Prelude hiding (fold)
import qualified Step.Internal.Prelude as Prelude

import qualified Seq

import Step.Nontrivial.Base (Nontrivial)
import qualified Step.Nontrivial.Base as Nontrivial
import qualified Step.Nontrivial.List as Nontrivial.List
import qualified Step.Nontrivial.SplitAtPositive as SplitAtPositive
import Step.Nontrivial.SplitAtPositive (splitAtPositive, SplitAtPositive)

import Step.Advancement (AdvanceResult, Progressive (..))
import qualified Step.Advancement as Advance

data Buffer text char = Buffer { chunks :: Seq (Nontrivial text char) }

instance Semigroup (Buffer text char) where
    a <> b = Buffer{ chunks = chunks a <> chunks b }

instance (Monad m, ListLike text char) => Progressive (StateT (Buffer text char) m) where
    advance n = get >>= \case
        Buffer{ chunks = Seq.Empty } -> return Advance.InsufficientInput{ Advance.shortfall = n }
        Buffer{ chunks = (Seq.:<|) x xs } -> case splitAtPositive n x of
            SplitAtPositive.All -> put Buffer{ chunks = xs } $> Advance.Success
            SplitAtPositive.Split _ b -> put Buffer{ chunks = (Seq.:<|) b xs } $> Advance.Success
            SplitAtPositive.Insufficient n' -> advance n'

singleton :: Nontrivial text char -> Buffer text char
singleton x = Buffer{ chunks = Seq.singleton x }

isEmpty :: Buffer text char -> Bool
isEmpty = Seq.null . chunks

empty :: Buffer text char
empty = Buffer{ chunks = Seq.empty }

toListT :: Monad m => Buffer text char -> ListT m (Nontrivial text char)
toListT = select . chunks

fold :: Monoid text => Buffer text char -> text
fold = Prelude.fold . fmap Nontrivial.generalize . chunks

headChar :: ListLike text char => Buffer text char -> Maybe char
headChar b =
    case chunks b of
        Seq.Empty -> Nothing
        (Seq.:<|) x _ -> let (c, _) = Nontrivial.uncons x in Just c

unconsChar :: ListLike text char => Buffer text char -> Maybe (char, Buffer text char)
unconsChar b =
    case chunks b of
        Seq.Empty -> Nothing
        (Seq.:<|) x xs -> let (c, x') = Nontrivial.uncons x in
            Just (c, Buffer{ chunks = Nontrivial.List.cons x' xs })

unconsChunk :: Buffer text char -> Maybe (Nontrivial text char, Buffer text char)
unconsChunk b = case uncons (chunks b) of
    Nothing -> Nothing
    Just (c, cs) -> Just (c, Buffer{ chunks = cs } )

data StripPrefixResult text char =
    StripPrefixFail
  | StripPrefixPartial (Nontrivial text char) -- ^ What further needed text remains
  | StripPrefixSuccess (Buffer text char) -- ^ What buffer is left after removing the text

stripPrefix :: (ListLike text char, Eq text, Eq char) => text -> Buffer text char -> StripPrefixResult text char
stripPrefix c b = case Nontrivial.refine c of
    Nothing -> StripPrefixSuccess b
    Just c' -> stripNontrivialPrefix c' b

stripNontrivialPrefix :: (ListLike text char, Eq text, Eq char) => Nontrivial text char -> Buffer text char -> StripPrefixResult text char
stripNontrivialPrefix c b = case chunks b of
    Seq.Empty -> StripPrefixPartial c
    (Seq.:<|) x xs -> case compare (Nontrivial.List.length x) (Nontrivial.List.length c) of
        EQ -> if x /= c then StripPrefixFail else
            StripPrefixSuccess Buffer{ chunks = xs }
        LT -> case Nontrivial.stripPrefix x c of
            Nothing -> StripPrefixFail
            Just c' -> stripPrefix c' Buffer{ chunks = xs }
        GT -> case Nontrivial.stripPrefix c x of
            Nothing -> StripPrefixFail
            Just x' -> case Nontrivial.refine x' of
                Nothing -> error "stripNontrivialPrefix: failure"
                Just x'' -> StripPrefixSuccess Buffer{ chunks = (Seq.<|) x'' xs }

takeChar :: (Monad m, ListLike text char) => StateT (Buffer text char) m (Maybe char)
takeChar = do
    b <- get
    case unconsChar b of
        Nothing -> return Nothing
        Just (c, b') -> do
            put b'
            return (Just c)

data TakeStringResult text char =
    TakeStringFail
  | TakeStringPartial (Nontrivial text char) -- ^ What further needed text remains
  | TakeStringSuccess

takeString :: (Monad m, ListLike text char, Eq text, Eq char) =>
    text -> StateT (Buffer text char) m (TakeStringResult text char)
takeString x = case Nontrivial.refine x of Nothing -> return TakeStringSuccess; Just y -> takeNontrivialString y

takeNontrivialString :: (Monad m, ListLike text char, Eq text, Eq char) =>
    Nontrivial text char -> StateT (Buffer text char) m (TakeStringResult text char)
takeNontrivialString c = do
    b <- get
    case stripNontrivialPrefix c b of
        StripPrefixFail -> return TakeStringFail
        StripPrefixPartial c' -> do
            put empty
            return (TakeStringPartial c')
        StripPrefixSuccess b' -> do
            put b'
            return TakeStringSuccess

takeChunk :: Monad m => StateT (Buffer text char) m (Maybe (Nontrivial text char))
takeChunk = do
    b <- get
    case unconsChunk b of
        Nothing -> return Nothing
        Just (c, b') -> do
            put b'
            return (Just c)

considerChunk :: Monad m => ListLike text char =>
    (Nontrivial text char -> (Natural, a)) -> StateT (Buffer text char) m (Maybe a)
considerChunk f = takeChunk >>= \case
    Nothing -> return Nothing
    Just x -> let (n, r) = f x in putChunk (Nontrivial.drop n x) $> Just r

-- | Adds a chunk back to the left side of the buffer if the argument is non-empty
putChunk :: Monad m => ListLike text char => text -> StateT (Buffer text char) m ()
putChunk = traverse_ putNontrivialChunk . Nontrivial.refine

-- | Adds a chunk back to the left side of the buffer
putNontrivialChunk :: Monad m => Nontrivial text char -> StateT (Buffer text char) m ()
putNontrivialChunk x = modify' (singleton x <>)
