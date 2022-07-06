module Step.Document.Prelude
  (
    {- * Single character result -} char, satisfy, satisfyJust, peekChar, peekCharMaybe,
    {- * Text result -} text, all,
    {- * Inspecting the position -} position, withLocation,
    {- * Repetition -} repetition0, repetition1, count0, count1,
    {- * The end -} atEnd, end,
    {- * Contextualizing errors -} contextualize, (<?>),
    {- * Failure -} failure, try,
    -- {- * Transformation -} under, while,
  )
  where

import Step.Internal.Prelude hiding (while, under, Is)

import Optics

import qualified NonEmpty

import Step.Document.Parser (Parser (Parser))
import qualified Step.Document.Parser as Parser

import qualified Loc
import Loc (Loc, SpanOrLoc)

import qualified Step.DocumentMemory.State as DocumentMemory.State

import qualified Step.Document.Config as Config

import qualified Step.Document.Do as P

import Step.Action.Safe hiding (try, failure)
import Step.Action.Kinds

import qualified Step.Action.Safe as Action

char :: Monad m => ListLike text char => Parser text MoveAtom m char
char = Parser $ MoveAtom \config ->
    DocumentMemory.State.takeChar <&> \case
        Nothing -> Left (Parser.makeError config)
        Just x -> Right x

peekChar :: Monad m => ListLike text char => Parser text Static m char
peekChar = Parser $ Static \config ->
    DocumentMemory.State.peekCharMaybe <&> \case
        Nothing -> Left (Parser.makeError config)
        Just x -> Right x

peekCharMaybe :: Monad m => ListLike text char => Parser text SureStatic m (Maybe char)
peekCharMaybe = Parser $ SureStatic \_ -> DocumentMemory.State.peekCharMaybe

satisfy :: Monad m => ListLike text char => (char -> Bool) -> Parser text MoveAtom m char
satisfy ok = Parser $ MoveAtom \config ->
    DocumentMemory.State.takeCharIf ok <&> \case
        Nothing -> Left (Parser.makeError config)
        Just x -> Right x

satisfyJust :: Monad m => ListLike text char => (char -> Maybe a) -> Parser text MoveAtom m a
satisfyJust ok = Parser $ MoveAtom \config ->
    DocumentMemory.State.takeCharJust ok <&> \case
        Nothing -> Left (Parser.makeError config)
        Just x -> Right x

text :: Monad m => ListLike text char => Eq text => Eq char => text -> Parser text Any m ()
text x = Parser $ Any \config ->
    DocumentMemory.State.takeText x <&> \case
        True -> Right ()
        False -> Left (Parser.makeError config)

atEnd :: Monad m => ListLike text char => Parser text SureStatic m Bool
atEnd = Parser $ SureStatic \_config -> DocumentMemory.State.atEnd

end :: Monad m => ListLike text char => Parser text Static m ()
end = Parser $ Static \config ->
    DocumentMemory.State.atEnd <&> \case
        True -> Right ()
        False -> Left (Parser.makeError config)

contextualize :: (Monad m, ConfigurableAction k, IsAction k) =>
    text -> Parser text k m a -> Parser text k m a
contextualize c (Parser p) = Parser $ configureAction (over Config.contextLens (c :)) p

infix 0 <?>
(<?>) :: (Monad m, ConfigurableAction k, IsAction k) => Parser text k m a -> text -> Parser text k m a
p <?> c = contextualize c p

position :: Monad m => ListLike text char => Parser text SureStatic m Loc
position = Parser $ SureStatic \_config -> DocumentMemory.State.getPosition

withLocation ::
    ListLike text char => Monad m => IsAction k =>
    Parser text k m a -> Parser text k m (SpanOrLoc, a)
withLocation p = P.do
    a <- position
    x <- p
    b <- position
    P.return (Loc.spanOrLocFromTo a b, x)

try :: Monad m => Atomic k => Parser text k m a -> Parser text (Action.Try k) m (Maybe a)
try (Parser p) = Parser (Action.try p)

repetition0 :: Monad m =>
    Parser text MoveAtom m a -> Parser text Sure m [a]
repetition0 p = fix \r -> P.do
    xm <- try p
    case xm of
        Nothing -> return []
        Just x -> (x :) <$> r

repetition1 :: Monad m =>
    Parser text MoveAtom m a -> Parser text Move m (NonEmpty a)
repetition1 p = P.do
    x <- p
    xs <- repetition0 p
    P.return (x :| xs)

count0 :: Monad m => Loop0 k k' =>
    Natural -> Parser text k m a -> Parser text k' m [a]
count0 = \n a -> go a n
  where
    go a = fix \r -> \case
        0 -> Parser (trivial [])
        n -> under (iso Parser (\(Parser z) -> z)) actionLiftTo P.do
            x <- a
            xs <- r (n - 1)
            P.return (x : xs)

count1 :: Monad m => Loop1 k k' =>
    Positive Natural -> Parser text k m a -> Parser text k' m (NonEmpty a)
count1 = \n a -> go a n
  where
    go a = fix \r -> \p ->
        case preview positive (review positive p - 1) of
            Nothing ->
                (:| []) <$> under (iso Parser (\(Parser z) -> z)) actionLiftTo a
            Just p' -> under (iso Parser (\(Parser z) -> z)) actionLiftTo P.do
                x <- a
                xs <- r p'
                P.return (NonEmpty.cons x xs)

failure :: Monad m => Action.CanFail k => Parser text k m a
failure = Parser $ Action.failure Parser.makeError

-- -- | Consume the rest of the input. This is mostly useful in conjunction with 'under'.
all :: Monad m => ListLike text char => Parser text Sure m text
all = Parser $ Sure \_config -> DocumentMemory.State.takeAll

-- under :: Monad m => ListLike text char => Transform text m text -> Parser text m a -> Parser text m a
-- under (Transform t) (Parser p) = Parser \eo -> do
--     s <- get
--     (s', x) <- zoom ParseState.futureLens $ lift $ runStateT (p eo) _
--     _

-- match :: ListLike text char => Monad m => Extent text m -> Parser text m text
-- match (Extent e) = Parser \_eo -> do
--     s <- get
--     (s', t) <- lift $ execStateT (match' e) (s, ListLike.empty)
--     put s'
--     return (Right (ListLike.fold t))

-- match' :: Monad m => ListLike text char =>
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

-- while :: ListLike text char => Monad m => (Char -> Bool) -> Transform text m text
-- while f = Transform while'
--   where
--     while' = ListT do
--         cm <- Stream.State.takeChunk
--         case cm of
--             Nothing -> return ListT.Nil
--             Just c | ListLike.null c -> return (ListT.Cons c while')
--             Just c -> do
--                 let (a, b) = ListLike.span f c
--                 Stream.State.putChunk b
--                 return if ListLike.null a then ListT.Nil else ListT.Cons a while'
