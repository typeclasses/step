module Stratoparsec.Buffer.State where

import ListLike (ListLike)
import Stratoparsec.Buffer.Base (Buffer)
import qualified Stratoparsec.Buffer.Base as Buffer

takeChar :: (Monad m, ListLike chunk char) => StateT (Buffer chunk) m (Maybe char)
takeChar = do
    b <- get
    case Buffer.uncons b of
        Nothing -> return Nothing
        Just (c, b') -> do
            put b'
            return (Just c)

data TakeStringResult chunk =
    TakeStringFail
  | TakeStringPartial chunk -- ^ What further needed text remains
  | TakeStringSuccess

takeString :: (Monad m, ListLike chunk char, Eq chunk, Eq char) =>
    chunk -> StateT (Buffer chunk) m (TakeStringResult chunk)
takeString c = do
    b <- get
    case Buffer.stripPrefix c b of
        Buffer.StripPrefixFail -> return TakeStringFail
        Buffer.StripPrefixPartial c' -> do
            put Buffer.empty
            return (TakeStringPartial c')
        Buffer.StripPrefixSuccess b' -> do
            put b'
            return TakeStringSuccess
