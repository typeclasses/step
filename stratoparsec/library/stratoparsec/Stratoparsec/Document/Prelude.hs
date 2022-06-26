module Stratoparsec.Document.Prelude where

import Optics

import Stratoparsec.Document.Parser

import qualified Stratoparsec.Document.ParseState as ParseState

import qualified Stratoparsec.Stream.State as Stream.State

import qualified Text

char :: Parser Char
char = Parser \eo -> do
    cm <- zoom ParseState.futureLens do
        Stream.State.fillBuffer 1
        Stream.State.takeChar
    case cm of
        Nothing -> let Parser f = failure in f eo
        Just x -> do
            ParseState.record (Text.singleton x)
            return (Right x)

text :: Text -> Parser ()
text expected = Parser \eo ->
    zoom ParseState.futureLens (Stream.State.takeString expected) >>= \case
        True -> do
            ParseState.record expected
            return (Right ())
        False -> let Parser p = failure in p eo

satisfy :: (Char -> Bool) -> Parser Char
satisfy ok = do
    x <- char
    case ok x of
        True -> return x
        False -> failure

atEnd :: Parser Bool
atEnd = Parser \_eo -> fmap Right $ zoom ParseState.futureLens Stream.State.isEmpty

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
