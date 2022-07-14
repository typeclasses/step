module Step.Cursor.Base
  (
    {- * The type -} Cursor (..),
    {- * Optics -} positionLens, bufferedStreamLens,
    {- * Conversion with ListT -} fromListT, toListT,
    {- * Buffer actions -} fillBuffer1, bufferMore, bufferAll,
    {- * Buffer inspection -} isAllBuffered, bufferIsEmpty, bufferHeadChar,
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

import Step.Nontrivial.Base (Nontrivial)
import qualified Step.Nontrivial.ListT as Nontrivial.ListT

data Cursor m text =
  Cursor
    { position :: CursorPosition
    , bufferedStream :: BufferedStream m text
    }

positionLens :: Lens' (Cursor m text) CursorPosition
positionLens = lens position \x y -> x{ position = y }

bufferIsEmpty :: Cursor m text -> Bool
bufferIsEmpty = BufferedStream.bufferIsEmpty . bufferedStream

isAllBuffered :: Cursor m text -> Bool
isAllBuffered = BufferedStream.isAllBuffered . bufferedStream

bufferAll :: Monad m => Cursor m text -> m (Cursor m text)
bufferAll = while (not . isAllBuffered) bufferMore

bufferedStreamLens :: Lens' (Cursor m text) (BufferedStream m text)
bufferedStreamLens = lens bufferedStream \x y -> x{ bufferedStream = y }

bufferUnconsChar :: ListLike text char => Cursor m text -> Maybe (char, Cursor m text)
bufferUnconsChar cbs = do
    (c, b') <- BufferedStream.bufferUnconsChar (bufferedStream cbs)
    Just (c, Cursor{ bufferedStream = b', position = position cbs + 1 })

bufferHeadChar :: Monad m => ListLike text char => Cursor m text -> Maybe char
bufferHeadChar = BufferedStream.bufferedHeadChar . bufferedStream

unconsChar :: Monad m => ListLike text char => Cursor m text -> m (Cursor m text, Maybe char)
unconsChar cbs = do
    cbs' <- fillBuffer1 cbs
    return case bufferUnconsChar cbs' of
        Nothing -> (cbs', Nothing)
        Just (x, cbs'') -> (cbs'', Just x)

unconsCharTentative :: Monad m => ListLike text char => Cursor m text -> m (Tentative.Step (Cursor m text) (Maybe char))
unconsCharTentative cbs = do
    cbs' <- fillBuffer1 cbs
    return  case bufferUnconsChar cbs' of
        Nothing -> Tentative.noChoiceStep cbs' Nothing
        Just (x, cbs'') -> Tentative.choiceStep
            Tentative.Choice{ Tentative.ifNotTaken = cbs', Tentative.ifActionTaken = cbs'' }
            (Just x)

fromListT :: Monad m => ListLike text char => ListT m text -> Cursor m text
fromListT xs = Cursor 0 (BufferedStream.fromListT xs)

toListT :: Monad m => Cursor m text -> ListT m (Nontrivial text)
toListT = BufferedStream.toListT . view bufferedStreamLens

fillBuffer1 :: Monad m => Cursor m text -> m (Cursor m text)
fillBuffer1 = traverseOf bufferedStreamLens BufferedStream.fillBuffer1

-- | Read one chunk of input. Does nothing if the end of the stream has been reached.
bufferMore :: Monad m => Cursor m text -> m (Cursor m text)
bufferMore = traverseOf bufferedStreamLens (BufferedStream.bufferMore)
