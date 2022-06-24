module Stratoparsec.Document.Prelude where

import Optics

import Stratoparsec.Document.Base

import qualified Stratoparsec.Stream.State as Stream.State

char :: Parser Char
char = Parser \eo -> do
    cm <- zoom futureLens do
        Stream.State.fillBuffer 1
        Stream.State.takeChar
    case cm of
        Nothing -> let Parser f = failure in f eo
        Just x -> return (Right x)

text :: Text -> Parser ()
text expected = Parser \eo ->
    zoom futureLens (Stream.State.takeString expected) >>= \case
        True -> return (Right ())
        False -> let Parser p = failure in p eo

satisfy :: (Char -> Bool) -> Parser Char
satisfy ok = do
    x <- char
    if ok x then return x else failure

atEnd :: Parser Bool
atEnd = Parser \_eo -> fmap Right $ zoom futureLens Stream.State.isEmpty

end :: Parser ()
end = atEnd >>= \case True -> return (); False -> failure

failure :: Parser a
failure = Parser \_eo ->
    return (Left (Error{ errorContext = [] }))

contextualize :: Context -> Parser a -> Parser a
contextualize c (Parser f) = Parser \eo ->
    f eo <&> \case
        Left (Error cs) -> Left (Error (c : cs))
        Right x -> Right x

(<?>) :: Parser a -> Context -> Parser a
p <?> c = contextualize c p
