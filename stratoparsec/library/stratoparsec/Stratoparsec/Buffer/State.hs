module Stratoparsec.Buffer.State where

import ListLike (ListLike)

import Stratoparsec.Buffer (Buffer)

import qualified Stratoparsec.Buffer as Buffer

takeChar :: (Monad m, ListLike chunk char) => StateT (Buffer chunk) m (Maybe char)
takeChar = do
    b <- get
    case Buffer.uncons b of
        Nothing -> return Nothing
        Just (c, b') -> do
            put b'
            return (Just c)
