module Step.CountingBufferedStream.Base
  (
    {- * The type -} CountingBufferedStream (..),
    {- * Optics -} positionLens, bufferLens, pendingLens, bufferedStreamLens,
    {- * Conversion with ListT -} fromListT, toListT,
    {- * Buffering -} fillBuffer, readChunk,
    {- * Taking from the stream -} unconsChar, bufferUnconsChar, peekChar,
  )
  where

import Step.Internal.Prelude

import Step.Buffer.Base (Buffer)
import qualified Step.Buffer.Base as Buffer

import Step.BufferedStream.Base (BufferedStream (BufferedStream))
import qualified Step.BufferedStream.Base as BufferedStream

data CountingBufferedStream m chunk =
  CountingBufferedStream
    { position :: Natural
    , buffer :: Buffer chunk
    , pending :: Maybe (ListT m chunk)
        -- ^ 'Nothing' indicates that the end of the stream has been reached.
    }

makeLensesFor
    [ ("position", "positionLens")
    , ("buffer", "bufferLens")
    , ("pending", "pendingLens")
    ]
    ''CountingBufferedStream

bufferedStreamLens :: Lens' (CountingBufferedStream m chunk) (BufferedStream m chunk)
bufferedStreamLens = lens
    (\x -> BufferedStream{ BufferedStream.buffer = buffer x, BufferedStream.pending = pending x })
    (\x bs -> x{ buffer = BufferedStream.buffer bs, pending = BufferedStream.pending bs })

bufferUnconsChar :: ListLike chunk char => CountingBufferedStream m chunk -> Maybe (char, CountingBufferedStream m chunk)
bufferUnconsChar cbs = case Buffer.unconsChar (buffer cbs) of
    Nothing -> Nothing
    Just (c, b') -> Just (c, cbs{ buffer = b', position = position cbs + 1 })

unconsChar :: Monad m => ListLike chunk char => CountingBufferedStream m chunk -> m (CountingBufferedStream m chunk, Maybe char)
unconsChar cbs = do
    cbs' <- fillBuffer 1 cbs
    return case bufferUnconsChar cbs' of
        Nothing -> (cbs', Nothing)
        Just (x, cbs'') -> (cbs'', Just x)

peekChar :: Monad m => ListLike chunk char => CountingBufferedStream m chunk ->
    m ( CountingBufferedStream m chunk           -- The stream if the peeked value is not taken
      , Maybe ( CountingBufferedStream m chunk   -- The stream if the peeked value is taken
              , char                             -- The peeked value
              )
      )
peekChar cbs = do
    cbs' <- fillBuffer 1 cbs
    return
      ( cbs'
      , case bufferUnconsChar cbs' of
          Nothing -> Nothing
          Just (x, cbs'') -> Just (cbs'', x)
      )

fromListT :: ListT m chunk -> CountingBufferedStream m chunk
fromListT xs =
    CountingBufferedStream 0 Buffer.empty (Just xs)

toListT :: Monad m => CountingBufferedStream m chunk -> ListT m chunk
toListT = BufferedStream.toListT . view bufferedStreamLens

-- | Force the input until at least @n@ characters of input are buffered or the end of input is reached.
fillBuffer :: (Monad m, ListLike chunk char) =>
    Natural -> CountingBufferedStream m chunk -> m (CountingBufferedStream m chunk)
fillBuffer n = traverseOf bufferedStreamLens (BufferedStream.fillBuffer n)

-- | Read one chunk of input. Does nothing if the end of the stream has been reached.
readChunk :: (Monad m, ListLike chunk char) =>
    CountingBufferedStream m chunk -> m (CountingBufferedStream m chunk)
readChunk = traverseOf bufferedStreamLens (BufferedStream.readChunk)
