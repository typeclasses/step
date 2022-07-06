module Step.BufferedStream.Base
  (
    {- * The type -} BufferedStream (..),
    {- * Optics -} bufferLens, pendingLens,
    {- * Constants -} empty,
    {- * Conversion with ListT -} toListT, fromListT,
    {- * Buffer querying -} bufferSize, bufferIsEmpty,
    {- * Buffer manipulation -} bufferUnconsChunk, putChunk,
    {- * Buffering -} fillBuffer, bufferMore,
  )
  where

import Step.Internal.Prelude

import qualified ListT

import Step.Buffer.Base (Buffer)
import qualified Step.Buffer.Base as Buffer

import Step.Nontrivial.Base (Nontrivial)
import qualified Step.Nontrivial.Base as Nontrivial
import qualified Step.Nontrivial.ListT as Nontrivial.ListT

data BufferedStream m text =
  BufferedStream
    { buffer :: Buffer text
    , pending :: Maybe (ListT m (Nontrivial text))
        -- ^ 'Nothing' indicates that the end of the stream has been reached.
    }

makeLensesFor [("buffer", "bufferLens"), ("pending", "pendingLens")] ''BufferedStream

empty :: BufferedStream m text
empty = BufferedStream Buffer.empty Nothing

toListT :: Monad m => BufferedStream m text -> ListT m (Nontrivial text)
toListT x = Buffer.toListT (buffer x) <|> asum (pending x)

fromListT :: ListLike text char => Monad m => ListT m text -> BufferedStream m text
fromListT x = BufferedStream{ buffer = Buffer.empty, pending = Just (Nontrivial.ListT.filter x) }

bufferSize :: BufferedStream m text -> Natural
bufferSize = Buffer.size . buffer

bufferIsEmpty :: BufferedStream m text -> Bool
bufferIsEmpty = Buffer.isEmpty . buffer

bufferUnconsChunk :: ListLike text char => BufferedStream m text -> Maybe (Nontrivial text, BufferedStream m text)
bufferUnconsChunk s = case Buffer.unconsChunk (buffer s) of
    Nothing -> Nothing
    Just (c, b') -> Just (c, s{ buffer = b' })

putChunk :: ListLike text char => text -> BufferedStream m text -> BufferedStream m text
putChunk x s = case Nontrivial.refine x of Nothing -> s; Just y -> putNontrivialChunk y s

putNontrivialChunk :: ListLike text char => Nontrivial text -> BufferedStream m text -> BufferedStream m text
putNontrivialChunk x s = s{ buffer = Buffer.singleton x <> buffer s }

-- | Force the input until at least @n@ characters of input are buffered or the end of input is reached.
fillBuffer :: (Monad m, ListLike text char) =>
    Natural -> BufferedStream m text -> m (BufferedStream m text)
fillBuffer n = while continue bufferMore
  where
    continue s =
        isJust (pending s)
        && Buffer.size (buffer s) < n

-- | Read one chunk of input. Does nothing if the end of the stream has been reached.
bufferMore :: (Monad m, ListLike text char) =>
    BufferedStream m text -> m (BufferedStream m text)
bufferMore s = case pending s of
    Nothing -> return s -- If the end of the stream has been reached, do nothing
    Just p ->
        ListT.next p -- Perform the next step in the pending input stream
        >>= \case
            ListT.Nil -> -- If the stream is now empty, change its value to 'Nothing' to remember that we have reached the end
                return s{ pending = Nothing }
            ListT.Cons x xs -> -- We got a new chunk of input.
                return BufferedStream{
                    buffer = buffer s <> Buffer.singleton x, -- Add the chunk to the buffer if it is non-empty.
                    pending = Just xs -- Remove the chunk from the pending input stream.
                }
