module Stratoparsec.Stream where

import ListT (ListT)
import Mono (MonoFoldable)

import qualified ListT

import Stratoparsec.Buffer (Buffer)
import qualified Stratoparsec.Buffer as Buffer

import Stratoparsec.Util.While

data Stream m chunk =
  Stream
    { buffer :: Buffer chunk
    , pending :: Maybe (ListT m chunk)
        -- ^ 'Nothing' indicates that the end of the stream has been reached.
    }

toListT :: Monad m => Stream m chunk -> ListT m chunk
toListT x = Buffer.toListT (buffer x) <|> asum (pending x)

fromListT :: ListT m chunk -> Stream m chunk
fromListT x = Stream{ buffer = Buffer.empty, pending = Just x }

bufferSize :: Stream m chunk -> Natural
bufferSize = Buffer.size . buffer

bufferIsEmpty :: Stream m chunk -> Bool
bufferIsEmpty = Buffer.isEmpty . buffer

-- | Force the input until at least @n@ characters of input are buffered or the end of input is reached.
fillBuffer :: (Monad m, MonoFoldable chunk) =>
    Natural -> Stream m chunk -> m (Stream m chunk)
fillBuffer n = while continue readChunk
  where
    continue s =
        isJust (pending s)
        && Buffer.size (buffer s) < n

-- | Read one chunk of input. Does nothing if the end of the stream has been reached.
readChunk :: (Monad m, MonoFoldable chunk) =>
    Stream m chunk -> m (Stream m chunk)
readChunk s = case pending s of
    Nothing -> return s -- If the end of the stream has been reached, do nothing
    Just p ->
        ListT.next p -- Perform the next step in the pending input stream
        >>= \case
            ListT.Nil -> -- If the stream is now empty, change its value to 'Nothing' to remember that we have reached the end
                return s{ pending = Nothing }
            ListT.Cons x xs -> -- We got a new chunk of input. Remove it from the pending input stream and add it to the buffer.
                return Stream{
                    buffer = buffer s <> Buffer.singleton x,
                    pending = Just xs }
