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
import qualified ListLike

import Step.Buffer.Base (Buffer)
import qualified Step.Buffer.Base as Buffer

import Step.Nontrivial.Base (Nontrivial)
import qualified Step.Nontrivial.Base as Nontrivial
import qualified Step.Nontrivial.ListT as Nontrivial.ListT

data BufferedStream m chunk =
  BufferedStream
    { buffer :: Buffer chunk
    , pending :: Maybe (ListT m (Nontrivial chunk))
        -- ^ 'Nothing' indicates that the end of the stream has been reached.
    }

makeLensesFor [("buffer", "bufferLens"), ("pending", "pendingLens")] ''BufferedStream

empty :: BufferedStream m chunk
empty = BufferedStream Buffer.empty Nothing

toListT :: Monad m => BufferedStream m chunk -> ListT m (Nontrivial chunk)
toListT x = Buffer.toListT (buffer x) <|> asum (pending x)

fromListT :: ListLike chunk char => Monad m => ListT m chunk -> BufferedStream m chunk
fromListT x = BufferedStream{ buffer = Buffer.empty, pending = Just (Nontrivial.ListT.filter x) }

bufferSize :: BufferedStream m chunk -> Natural
bufferSize = Buffer.size . buffer

bufferIsEmpty :: BufferedStream m chunk -> Bool
bufferIsEmpty = Buffer.isEmpty . buffer

bufferUnconsChunk :: ListLike chunk char => BufferedStream m chunk -> Maybe (Nontrivial chunk, BufferedStream m chunk)
bufferUnconsChunk s = case Buffer.unconsChunk (buffer s) of
    Nothing -> Nothing
    Just (c, b') -> Just (c, s{ buffer = b' })

putChunk :: ListLike chunk char => chunk -> BufferedStream m chunk -> BufferedStream m chunk
putChunk x s = case Nontrivial.refine x of Nothing -> s; Just y -> putNontrivialChunk y s

putNontrivialChunk :: ListLike chunk char => Nontrivial chunk -> BufferedStream m chunk -> BufferedStream m chunk
putNontrivialChunk x s = s{ buffer = Buffer.singleton x <> buffer s }

-- | Force the input until at least @n@ characters of input are buffered or the end of input is reached.
fillBuffer :: (Monad m, ListLike chunk char) =>
    Natural -> BufferedStream m chunk -> m (BufferedStream m chunk)
fillBuffer n = while continue bufferMore
  where
    continue s =
        isJust (pending s)
        && Buffer.size (buffer s) < n

-- | Read one chunk of input. Does nothing if the end of the stream has been reached.
bufferMore :: (Monad m, ListLike chunk char) =>
    BufferedStream m chunk -> m (BufferedStream m chunk)
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
