module Stratoparsec.Document.Prelude where

import Optics

import Stratoparsec.Document

import qualified Stratoparsec.Stream.State as Stream.State

char :: Monad m => Parser m Char
char = Parser \eo -> do
    cm <- zoom futureLens do
        Stream.State.fillBuffer 1
        Stream.State.takeChar
    case cm of
        Nothing -> let Parser f = failure in f eo
        Just x -> return (Right x)

satisfy :: Monad m => (Char -> Bool) -> Parser m Char
satisfy ok = do
    x <- char
    if ok x then return x else failure

atEnd :: Monad m => Parser m Bool
atEnd = Parser \_eo -> fmap Right $ zoom futureLens Stream.State.isEmpty

end :: Monad m => Parser m ()
end = atEnd >>= \case True -> return (); False -> failure

failure :: Monad m => Parser m a
failure = Parser \_eo -> do
    c <- use contextStackLens
    return (Left (Error c))
