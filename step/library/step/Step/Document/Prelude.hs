{-# language FlexibleContexts, QualifiedDo, TypeFamilies #-}

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

import Step.Internal.Prelude hiding (while, under)

import Optics

import qualified NonEmpty

import Step.Document.Parser (Parser (Parser), cast)
import qualified Step.Document.Parser as Parser

import qualified Loc
import Loc (Loc, SpanOrLoc)

import qualified Step.DocumentMemory.State as DocumentMemory.State

import qualified Step.Document.Config as Config

import qualified Step.Document.Do as P

import Step.ActionTypes (Atomic, Loop0, Loop1)
import qualified Step.ActionTypes as Action
import Step.ActionTypes.Types

import qualified Step.ActionTypes.Unsafe as Action.Unsafe

char :: Monad m => ListLike text char => Parser text AtomicMove m char
char = Parser \c -> Action.Unsafe.AtomicMove $
    DocumentMemory.State.takeChar <&> \case
        Nothing -> Left (Parser.makeError c)
        Just x -> Right x

peekChar :: Monad m => ListLike text char => Parser text Query m char
peekChar = Parser \c -> Action.Unsafe.Query $
    DocumentMemory.State.peekCharMaybe <&> \case
        Nothing -> Left (Parser.makeError c)
        Just x -> Right x

peekCharMaybe :: Monad m => ListLike text char => Parser text SureQuery m (Maybe char)
peekCharMaybe = Parser \_ -> Action.Unsafe.SureQuery DocumentMemory.State.peekCharMaybe

satisfy :: Monad m => ListLike text char => (char -> Bool) -> Parser text AtomicMove m char
satisfy ok = Parser \c -> Action.Unsafe.AtomicMove $
    DocumentMemory.State.takeCharIf ok <&> \case
        Nothing -> Left (Parser.makeError c)
        Just x -> Right x

satisfyJust :: Monad m => ListLike text char => (char -> Maybe a) -> Parser text AtomicMove m a
satisfyJust ok = Parser \c -> Action.Unsafe.AtomicMove $
    DocumentMemory.State.takeCharJust ok <&> \case
        Nothing -> Left (Parser.makeError c)
        Just x -> Right x

text :: Monad m => ListLike text char => Eq text => Eq char => text -> Parser text Any m ()
text x = Parser \c -> Action.Unsafe.Any $
    DocumentMemory.State.takeText x <&> \case
        True -> Right ()
        False -> Left (Parser.makeError c)

atEnd :: Monad m => ListLike text char => Parser text SureQuery m Bool
atEnd = Parser \_ -> Action.Unsafe.SureQuery DocumentMemory.State.atEnd

end :: Monad m => ListLike text char => Parser text Query m ()
end = Parser \c -> Action.Unsafe.Query $
    DocumentMemory.State.atEnd <&> \case
        True -> Right ()
        False -> Left (Parser.makeError c)

contextualize :: Monad m => text -> Parser text k m a -> Parser text k m a
contextualize n (Parser p) = Parser \c -> p (over Config.contextLens (n :) c)

infix 0 <?>
(<?>) :: Monad m => Parser text k m a -> text -> Parser text k m a
p <?> c = contextualize c p

position :: Monad m => ListLike text char => Parser text SureQuery m Loc
position = Parser $ \_ -> Action.Unsafe.SureQuery DocumentMemory.State.getPosition

withLocation ::
    ListLike text char => Monad m =>
    Action.Join SureQuery k =>
    Action.Join k SureQuery =>
    Parser text k m a -> Parser text k m (SpanOrLoc, a)
withLocation p =
    (\a x b -> (Loc.spanOrLocFromTo a b, x)) P.<$> position P.<*> p P.<*> position

try :: Monad m => Atomic k1 k2 => Parser text k1 m a -> Parser text k2 m (Maybe a)
try (Parser p) = Parser \c -> Action.try (p c)

repetition0 :: Monad m => Parser text AtomicMove m a -> Parser text Sure m [a]
repetition0 p = fix \r -> P.do
    xm <- try p
    case xm of
        Nothing -> return []
        Just x -> (x :) <$> r

repetition1 :: Monad m => Parser text AtomicMove m a -> Parser text AtomicMove m (NonEmpty a)
repetition1 p = P.do
    x <- p
    xs <- repetition0 p
    P.return (x :| xs)

count0 :: Monad m => Loop0 k k' => Natural -> Parser text k m a -> Parser text k' m [a]
count0 = \n a -> go a n
  where
    go a = fix \r -> \case
        0 -> Parser \_ -> Action.trivial []
        n -> cast ((:) P.<$> a P.<*> (r (n - 1)))

count1 :: Monad m => Loop1 k k' =>
    Positive Natural -> Parser text k m a -> Parser text k' m (NonEmpty a)
count1 = \n a -> go a n
  where
    go a = fix \r -> \p ->
        case preview positive (review positive p - 1) of
            Nothing -> (:| []) <$> cast a
            Just p' -> cast (NonEmpty.cons P.<$> a P.<*> r p')

failure :: Monad m => Parser text Fail m a
failure = Parser \c -> Action.Unsafe.Fail (Parser.makeError c)

-- -- | Consume the rest of the input. This is mostly useful in conjunction with 'under'.
all :: Monad m => ListLike text char => Parser text Sure m text
all = Parser \_ -> Action.Unsafe.Sure DocumentMemory.State.takeAll

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
