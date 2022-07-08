{-# language Safe #-}

module Step.Buffer.Base where

import Step.Internal.Prelude
import qualified Step.Internal.Prelude as Prelude

import qualified Seq

import Step.Nontrivial.Base (Nontrivial)
import qualified Step.Nontrivial.Base as Nontrivial
import qualified Step.Nontrivial.List as Nontrivial.List

data Buffer text =
  Buffer
    { chunks :: Seq (Nontrivial text)
    , size :: Natural
    }

instance Semigroup (Buffer text) where
    a <> b = Buffer{ chunks = chunks a <> chunks b,
                     size = size a + size b }

singleton :: ListLike text char => Nontrivial text -> Buffer text
singleton x =
    Buffer{ chunks = Seq.singleton x, size = Nontrivial.length x }

isEmpty :: Buffer text -> Bool
isEmpty = (== 0) . size

empty :: Buffer text
empty = Buffer{ chunks = Seq.empty, size = 0 }

toListT :: Monad m => Buffer a -> ListT m (Nontrivial a)
toListT = select . chunks

fold :: Monoid text => Buffer text -> text
fold = Prelude.fold . fmap Nontrivial.generalize . chunks

headChar :: ListLike text char => Buffer text -> Maybe char
headChar b =
    case chunks b of
        Seq.Empty -> Nothing
        (Seq.:<|) x _ -> let (c, _) = Nontrivial.uncons x in Just c

unconsChar :: ListLike text char => Buffer text -> Maybe (char, Buffer text)
unconsChar b =
    case chunks b of
        Seq.Empty -> Nothing
        (Seq.:<|) x xs -> let (c, x') = Nontrivial.uncons x in
            Just (c, Buffer{
                chunks = Nontrivial.List.cons x' xs,
                size = size b - 1
            })

unconsChunk :: ListLike text char => Buffer text -> Maybe (Nontrivial text, Buffer text)
unconsChunk b = case uncons (chunks b) of
    Nothing -> Nothing
    Just (c, cs) -> Just (c, Buffer{ chunks = cs, size = size b - Nontrivial.length c } )

data StripPrefixResult text =
    StripPrefixFail
  | StripPrefixPartial (Nontrivial text) -- ^ What further needed text remains
  | StripPrefixSuccess (Buffer text) -- ^ What buffer is left after removing the text

stripPrefix :: (ListLike text char, Eq text, Eq char) => text -> Buffer text -> StripPrefixResult text
stripPrefix c b = case Nontrivial.refine c of
    Nothing -> StripPrefixSuccess b
    Just c' -> stripNontrivialPrefix c' b

stripNontrivialPrefix :: (ListLike text char, Eq text, Eq char) => Nontrivial text -> Buffer text -> StripPrefixResult text
stripNontrivialPrefix c b = case chunks b of
    Seq.Empty -> StripPrefixPartial c
    (Seq.:<|) x xs -> case compare (Nontrivial.length x) (Nontrivial.length c) of
        EQ -> if x /= c then StripPrefixFail else
            StripPrefixSuccess Buffer{ chunks = xs, size = size b - Nontrivial.length x }
        LT -> case Nontrivial.stripPrefix x c of
            Nothing -> StripPrefixFail
            Just c' -> stripPrefix c' Buffer{ chunks = xs, size = size b - Nontrivial.length x }
        GT -> case Nontrivial.stripPrefix c x of
            Nothing -> StripPrefixFail
            Just x' -> case Nontrivial.refine x' of
                Nothing -> error "stripNontrivialPrefix: failure"
                Just x'' -> StripPrefixSuccess Buffer{ chunks = (Seq.<|) x'' xs, size = size b - Nontrivial.length c }
