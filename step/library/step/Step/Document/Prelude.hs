module Step.Document.Prelude
  (
    {- * Single character result -} char, satisfy,
    {- * Text result -} text, all,
    {- * Inspecting the position -} position, withLocation,
    {- * Possibility to Parser -} many, require,
    {- * The end -} atEnd, end,
    {- * Contextualizing errors -} contextualize, (<?>),
    {- * Failure -} failure,
    {- * Transformation -} under, while,
  )
  where

import Step.Internal.Prelude hiding (while, under)

import Step.Document.Parser

import qualified Loc
import Loc (Loc, SpanOrLoc)

import Step.Document.ParseState (ParseState (ParseState))
import qualified Step.Document.ParseState as ParseState

import Step.Buffer.Base (Buffer)
import qualified Step.Buffer.Base as Buffer

import Step.Stream.Base (Stream)
import qualified Step.Stream.Base as Stream

import qualified Step.Stream.State as Stream.State

import qualified Step.Document.Past as Past

import qualified ListLike

import qualified ListT
import ListT (ListT (ListT))

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

-- | Consume the rest of the input. This is mostly useful in conjunction with 'under'.
all :: Monad m => ListLike text Char => Parser text m text
all = Parser \_eo -> do
    zoom ParseState.futureLens Stream.State.bufferAll
    xs <- Buffer.chunks <$> use (ParseState.futureLens % Stream.bufferLens)
    assign ParseState.futureLens Stream.empty
    zoom ParseState.pastLens $ traverse_ (modify' . Past.record) xs
    return (Right (ListLike.fold xs))

under :: Monad m => ListLike text Char => Transform text m text -> Parser text m a -> Parser text m a
under (Transform t) (Parser p) = Parser \eo -> do
    s <- get
    (s', x) <- zoom ParseState.futureLens $ lift $ runStateT (p eo) _
    _

-- match :: ListLike text Char => Monad m => Extent text m -> Parser text m text
-- match (Extent e) = Parser \_eo -> do
--     s <- get
--     (s', t) <- lift $ execStateT (match' e) (s, ListLike.empty)
--     put s'
--     return (Right (ListLike.fold t))

-- match' :: Monad m => ListLike text Char =>
--     ListT (StateT (Stream m text) m) text
--     -> StateT (ParseState text m, Seq text) m ()
-- match' e = do
--     step <- zoom (_1 % ParseState.futureLens) (ListT.next e)
--     case step of
--         ListT.Nil -> return ()
--         ListT.Cons x e' -> do
--             zoom _1 (ParseState.record x)
--             modifying _2 (`ListLike.snoc` x)
--             match' e'

while :: ListLike text Char => Monad m => (Char -> Bool) -> Transform text m text
while f = Transform while'
  where
    while' = ListT do
        cm <- Stream.State.takeChunk
        case cm of
            Nothing -> return ListT.Nil
            Just c | ListLike.null c -> return (ListT.Cons c while')
            Just c -> do
                let (a, b) = ListLike.span f c
                Stream.State.putChunk b
                return if ListLike.null a then ListT.Nil else ListT.Cons a while'
