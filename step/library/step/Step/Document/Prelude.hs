module Step.Document.Prelude where

import Optics

import Step.Document.Parser

import Step.Document.Position (Position)

import qualified Step.Document.ParseState as ParseState

import qualified Step.Stream.State as Stream.State

import qualified ListLike

char :: ListLike text Char => Parser text Char
char = Parser \eo -> do
    cm <- zoom ParseState.futureLens do
        Stream.State.fillBuffer 1
        Stream.State.takeChar
    case cm of
        Nothing -> let Parser f = failure in f eo
        Just x -> do
            ParseState.record (ListLike.singleton x)
            return (Right x)

text :: ListLike text Char => Eq text => text -> Parser text ()
text expected = Parser \eo ->
    zoom ParseState.futureLens (Stream.State.takeString expected) >>= \case
        True -> do
            ParseState.record expected
            return (Right ())
        False -> let Parser p = failure in p eo

satisfy :: ListLike text Char => (Char -> Bool) -> Parser text Char
satisfy ok = do
    x <- char
    case ok x of
        True -> return x
        False -> failure

position :: Parser text Position
position = Parser \_eo -> use ParseState.positionLens <&> Right

atEnd :: ListLike text Char => Parser text Bool
atEnd = Parser \_eo -> fmap Right $ zoom ParseState.futureLens Stream.State.isEmpty

end :: ListLike text Char => Parser text ()
end = atEnd >>= \case True -> return (); False -> failure

failure :: Parser text a
failure = Parser \_eo ->
    return (Left (Error{ errorContext = [] }))

contextualize :: Context text -> Parser text a -> Parser text a
contextualize c (Parser f) = Parser \eo ->
    f eo <&> \case
        Left (Error cs) -> Left (Error (c : cs))
        Right x -> Right x

(<?>) :: Parser text a -> Context text -> Parser text a
p <?> c = contextualize c p
