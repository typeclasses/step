module Step.Buffer.Base where

import Step.Internal.Prelude

import qualified Seq
import qualified ListLike
import qualified ListT

import Step.Nontrivial.Base (Nontrivial)
import qualified Step.Nontrivial.Base as Nontrivial
import qualified Step.Nontrivial.List as Nontrivial.List

data Buffer chunk =
  Buffer
    { chunks :: Seq (Nontrivial chunk)
    , size :: Natural
    }

instance Semigroup (Buffer chunk) where
    a <> b = Buffer{ chunks = chunks a <> chunks b,
                     size = size a + size b }

singleton :: ListLike chunk char => Nontrivial chunk -> Buffer chunk
singleton x =
    Buffer{ chunks = Seq.singleton x, size = Nontrivial.length x }

isEmpty :: Buffer chunk -> Bool
isEmpty = (== 0) . size

empty :: Buffer chunk
empty = Buffer{ chunks = Seq.empty, size = 0 }

toListT :: Monad m => Buffer a -> ListT m (Nontrivial a)
toListT = ListT.select . chunks

fold :: Monoid chunk => Buffer chunk -> chunk
fold = ListLike.fold . fmap Nontrivial.generalize . chunks

unconsChar :: ListLike chunk char => Buffer chunk -> Maybe (char, Buffer chunk)
unconsChar b = if isEmpty b then Nothing else Just $
    case chunks b of
        Seq.Empty -> error "Buffer size is 0 but it has no chunks"
        (Seq.:<|) x xs -> let (c, x') = Nontrivial.uncons x in
            (c, Buffer{
                chunks = Nontrivial.List.cons x' xs,
                size = size b - 1
            })

unconsChunk :: ListLike chunk char => Buffer chunk -> Maybe (Nontrivial chunk, Buffer chunk)
unconsChunk b = case ListLike.uncons (chunks b) of
    Nothing -> Nothing
    Just (c, cs) -> Just (c, Buffer{ chunks = cs, size = size b - Nontrivial.length c } )

data StripPrefixResult chunk =
    StripPrefixFail
  | StripPrefixPartial (Nontrivial chunk) -- ^ What further needed text remains
  | StripPrefixSuccess (Buffer chunk) -- ^ What buffer is left after removing the text

stripPrefix :: (ListLike chunk char, Eq chunk, Eq char) => chunk -> Buffer chunk -> StripPrefixResult chunk
stripPrefix c b = case Nontrivial.refine c of
    Nothing -> StripPrefixSuccess b
    Just c' -> stripNontrivialPrefix c' b

stripNontrivialPrefix :: (ListLike chunk char, Eq chunk, Eq char) => Nontrivial chunk -> Buffer chunk -> StripPrefixResult chunk
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
