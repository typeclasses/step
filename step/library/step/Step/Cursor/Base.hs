module Step.Cursor.Base
  (
    {- * The type -} Cursor (..),
    {- * Optics -} positionLens, bufferLens, pendingLens, bufferedStreamLens,
    {- * Conversion with ListT -} fromListT, toListT,
    {- * Buffer actions -} fillBuffer, bufferMore, bufferAll,
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
    , buffer :: Buffer text
    , pending :: Maybe (ListT m (Nontrivial text))
        -- ^ 'Nothing' indicates that the end of the stream has been reached.
    }

makeLensesFor
    [ ("position", "positionLens")
    , ("buffer", "bufferLens")
    , ("pending", "pendingLens")
    ]
    ''Cursor

bufferIsEmpty :: Cursor m text -> Bool
bufferIsEmpty = Buffer.isEmpty . buffer

isAllBuffered :: Cursor m text -> Bool
isAllBuffered = isNothing . pending

bufferAll :: Monad m => ListLike text char => Cursor m text -> m (Cursor m text)
bufferAll = while (not . isAllBuffered) bufferMore

bufferedStreamLens :: Lens' (Cursor m text) (BufferedStream m text)
bufferedStreamLens = lens
    (\x -> BufferedStream{ BufferedStream.buffer = buffer x, BufferedStream.pending = pending x })
    (\x bs -> x{ buffer = BufferedStream.buffer bs, pending = BufferedStream.pending bs })

bufferUnconsChar :: ListLike text char => Cursor m text -> Maybe (char, Cursor m text)
bufferUnconsChar cbs = case Buffer.unconsChar (buffer cbs) of
    Nothing -> Nothing
    Just (c, b') -> Just (c, cbs{ buffer = b', position = position cbs + 1 })

bufferHeadChar :: Monad m => ListLike text char => Cursor m text -> Maybe char
bufferHeadChar = Buffer.headChar . buffer

unconsChar :: Monad m => ListLike text char => Cursor m text -> m (Cursor m text, Maybe char)
unconsChar cbs = do
    cbs' <- fillBuffer 1 cbs
    return case bufferUnconsChar cbs' of
        Nothing -> (cbs', Nothing)
        Just (x, cbs'') -> (cbs'', Just x)

unconsCharTentative :: Monad m => ListLike text char => Cursor m text -> m (Tentative.Step (Cursor m text) (Maybe char))
unconsCharTentative cbs = do
    cbs' <- fillBuffer 1 cbs
    return  case bufferUnconsChar cbs' of
        Nothing -> Tentative.noChoiceStep cbs' Nothing
        Just (x, cbs'') -> Tentative.choiceStep
            Tentative.Choice{ Tentative.ifNotTaken = cbs', Tentative.ifActionTaken = cbs'' }
            (Just x)

fromListT :: Monad m => ListLike text char => ListT m text -> Cursor m text
fromListT xs =
    Cursor 0 Buffer.empty (Just (Nontrivial.ListT.filter xs))

toListT :: Monad m => Cursor m text -> ListT m (Nontrivial text)
toListT = BufferedStream.toListT . view bufferedStreamLens

-- | Force the input until at least @n@ characters of input are buffered or the end of input is reached.
fillBuffer :: (Monad m, ListLike text char) =>
    Natural -> Cursor m text -> m (Cursor m text)
fillBuffer n = traverseOf bufferedStreamLens (BufferedStream.fillBuffer n)

-- | Read one chunk of input. Does nothing if the end of the stream has been reached.
bufferMore :: (Monad m, ListLike text char) =>
    Cursor m text -> m (Cursor m text)
bufferMore = traverseOf bufferedStreamLens (BufferedStream.bufferMore)
