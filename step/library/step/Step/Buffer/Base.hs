module Step.Buffer.Base where

import qualified Seq
import qualified ListLike
import qualified ListT

data Buffer chunk =
  Buffer
    { chunks :: Seq chunk -- ^ A chunk in this list should never be empty.
    , size :: Natural
    }

instance Semigroup (Buffer chunk) where
    a <> b = Buffer{ chunks = chunks a <> chunks b,
                     size = size a + size b }

singleton :: ListLike chunk char => chunk -> Buffer chunk
singleton x =
    Buffer{ chunks = Seq.singleton x, size = fromIntegral $ ListLike.length x }

isEmpty :: Buffer chunk -> Bool
isEmpty = (== 0) . size

empty :: Buffer chunk
empty = Buffer{ chunks = Seq.empty, size = 0 }

toListT :: Monad m => Buffer a -> ListT m a
toListT = ListT.select . chunks

uncons :: ListLike chunk char => Buffer chunk -> Maybe (char, Buffer chunk)
uncons b = if isEmpty b then Nothing else Just $
    case chunks b of
        Seq.Empty -> error "Buffer size is 0 but it has no chunks"
        (Seq.:<|) x xs ->
              case ListLike.uncons x of
                  Nothing -> error "Buffer contains an empty chunk"
                  Just (c, x') -> (c, Buffer{
                      chunks = (if ListLike.null x' then id else (x' Seq.<|)) xs,
                      size = size b - 1
                  })

data StripPrefixResult chunk =
    StripPrefixFail
  | StripPrefixPartial chunk -- ^ What further needed text remains
  | StripPrefixSuccess (Buffer chunk) -- ^ What buffer is left after removing the text

stripPrefix :: (ListLike chunk char, Eq chunk, Eq char) => chunk -> Buffer chunk -> StripPrefixResult chunk
stripPrefix c b =
    if ListLike.null c then StripPrefixSuccess b else
    case chunks b of
        Seq.Empty -> StripPrefixPartial c
        (Seq.:<|) x xs -> case compare (ListLike.length x) (ListLike.length c) of
            EQ -> if x /= c then StripPrefixFail else
                StripPrefixSuccess Buffer{ chunks = xs, size = size b - fromIntegral (ListLike.length x) }
            LT -> case ListLike.stripPrefix x c of
                Nothing -> StripPrefixFail
                Just c' -> stripPrefix c' Buffer{ chunks = xs, size = size b - fromIntegral (ListLike.length x) }
            GT -> case ListLike.stripPrefix c x of
                Nothing -> StripPrefixFail
                Just x' -> StripPrefixSuccess Buffer{ chunks = (Seq.<|) x' xs, size = size b - fromIntegral (ListLike.length c) }
