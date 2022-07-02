module Step.Cursor.Base
  (
    {- * The type -} Cursor (..),
    {- * Optics -} positionLens, bufferLens, pendingLens, bufferedStreamLens,
    {- * Conversion with ListT -} fromListT, toListT,
    {- * Buffering -} fillBuffer, readChunk, bufferAll, isAllBuffered, bufferIsEmpty,
    {- * Taking from the stream -} unconsChar, bufferUnconsChar, unconsCharTentative,
  )
  where

import Step.Internal.Prelude

import Step.Buffer.Base (Buffer)
import qualified Step.Buffer.Base as Buffer

import Step.BufferedStream.Base (BufferedStream (BufferedStream))
import qualified Step.BufferedStream.Base as BufferedStream

import qualified Step.Tentative.Base as Tentative

import Step.CursorPosition.Base (CursorPosition)

data Cursor m chunk =
  Cursor
    { position :: CursorPosition
    , buffer :: Buffer chunk
    , pending :: Maybe (ListT m chunk)
        -- ^ 'Nothing' indicates that the end of the stream has been reached.
    }

makeLensesFor
    [ ("position", "positionLens")
    , ("buffer", "bufferLens")
    , ("pending", "pendingLens")
    ]
    ''Cursor

bufferIsEmpty :: Cursor m chunk -> Bool
bufferIsEmpty = Buffer.isEmpty . buffer

isAllBuffered :: Cursor m chunk -> Bool
isAllBuffered = isNothing . pending

bufferAll :: Monad m => ListLike chunk char => Cursor m chunk -> m (Cursor m chunk)
bufferAll = while (not . isAllBuffered) readChunk

bufferedStreamLens :: Lens' (Cursor m chunk) (BufferedStream m chunk)
bufferedStreamLens = lens
    (\x -> BufferedStream{ BufferedStream.buffer = buffer x, BufferedStream.pending = pending x })
    (\x bs -> x{ buffer = BufferedStream.buffer bs, pending = BufferedStream.pending bs })

bufferUnconsChar :: ListLike chunk char => Cursor m chunk -> Maybe (char, Cursor m chunk)
bufferUnconsChar cbs = case Buffer.unconsChar (buffer cbs) of
    Nothing -> Nothing
    Just (c, b') -> Just (c, cbs{ buffer = b', position = position cbs + 1 })

unconsChar :: Monad m => ListLike chunk char => Cursor m chunk -> m (Cursor m chunk, Maybe char)
unconsChar cbs = do
    cbs' <- fillBuffer 1 cbs
    return case bufferUnconsChar cbs' of
        Nothing -> (cbs', Nothing)
        Just (x, cbs'') -> (cbs'', Just x)

unconsCharTentative :: Monad m => ListLike chunk char => Cursor m chunk -> m (Tentative.Step (Cursor m chunk) (Maybe char))
unconsCharTentative cbs = do
    cbs' <- fillBuffer 1 cbs
    return  case bufferUnconsChar cbs' of
        Nothing -> Tentative.noChoiceStep cbs' Nothing
        Just (x, cbs'') -> Tentative.choiceStep
            Tentative.Choice{ Tentative.ifNotTaken = cbs', Tentative.ifActionTaken = cbs'' }
            (Just x)

fromListT :: ListT m chunk -> Cursor m chunk
fromListT xs =
    Cursor 0 Buffer.empty (Just xs)

toListT :: Monad m => Cursor m chunk -> ListT m chunk
toListT = BufferedStream.toListT . view bufferedStreamLens

-- | Force the input until at least @n@ characters of input are buffered or the end of input is reached.
fillBuffer :: (Monad m, ListLike chunk char) =>
    Natural -> Cursor m chunk -> m (Cursor m chunk)
fillBuffer n = traverseOf bufferedStreamLens (BufferedStream.fillBuffer n)

-- | Read one chunk of input. Does nothing if the end of the stream has been reached.
--
-- todo: rename to "bufferMore"
readChunk :: (Monad m, ListLike chunk char) =>
    Cursor m chunk -> m (Cursor m chunk)
readChunk = traverseOf bufferedStreamLens (BufferedStream.readChunk)
