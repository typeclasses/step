module Step.Document.Prelude where

import Step.Internal.Prelude

import Step.Document.Parser

import qualified Loc
import Loc (Loc, SpanOrLoc)

import Step.Document.ParseState (ParseState (ParseState))
import qualified Step.Document.ParseState as ParseState

import qualified Step.Stream.State as Stream.State

import qualified Step.Document.Past as Past

import qualified ListLike

char :: Monad m => ListLike text Char => Possibility text m Char
char = satisfy (\_ -> True)

satisfy :: Monad m => ListLike text Char => (Char -> Bool) -> Possibility text m Char
satisfy ok =  Possibility \_eo s -> do
    (cm, fu) <- runStateT
        do
            Stream.State.fillBuffer 1
            Stream.State.takeChar
        (ParseState.future s)
    case cm of
        Just x | ok x ->
            return $ Right
              ( ParseState{ ParseState.past = Past.record (ListLike.singleton x) (ParseState.past s)
                          , ParseState.future = fu
                          }
              , x
              )
        _ -> return (Left fu)

text :: Monad m => ListLike text Char => Eq text => text -> Parser text m ()
text expected = Parser \eo ->
    zoom ParseState.futureLens (Stream.State.takeString expected) >>= \case
        True -> do
            ParseState.record expected
            return (Right ())
        False -> let Parser p = failure in p eo

position :: Monad m => Parser text m Loc
position = Parser \_eo -> use ParseState.positionLens <&> Right

atEnd :: Monad m => ListLike text Char => Parser text m Bool
atEnd = Parser \_eo -> fmap Right $ zoom ParseState.futureLens Stream.State.isEmpty

end :: Monad m => ListLike text Char => Parser text m ()
end = atEnd >>= \case True -> return (); False -> failure

failure :: Monad m => Parser text m a
failure = Parser \_eo ->
    return (Left (Error{ errorContext = [] }))

contextualize :: Monad m => Context text -> Parser text m a -> Parser text m a
contextualize c (Parser f) = Parser \eo ->
    f eo <&> \case
        Left (Error cs) -> Left (Error (c : cs))
        Right x -> Right x

(<?>) :: Monad m => Parser text m a -> Context text -> Parser text m a
p <?> c = contextualize c p

withLocation :: Monad m => Parser text m a -> Parser text m (SpanOrLoc, a)
withLocation p = do
    a <- position
    x <- p
    b <- position
    return (Loc.spanOrLocFromTo a b, x)

many :: Monad m => ListLike list a => Possibility text m a -> Parser text m list
many (Possibility p) = Parser \eo ->
  let
    r s = do
      result <- p eo s
      case result of
          Left fu -> return (s{ ParseState.future = fu }, ListLike.empty)
          Right (s', x) -> (over _2 (ListLike.singleton x <>)) <$> r s'
  in
    do
      s <- get
      (s', xs) <- lift $ r s
      put s'
      return (Right xs)

require :: Monad m => Possibility text m a -> Parser text m a
require (Possibility p) = Parser \eo -> do
    s <- get
    result <- lift (p eo s)
    case result of
        Left _ -> let Parser f = failure in f eo
        Right (s', x) -> do
            put s'
            return (Right x)
