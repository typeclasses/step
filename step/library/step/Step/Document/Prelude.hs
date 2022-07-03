module Step.Document.Prelude
  (
    {- * Single character result -} char, satisfy,
    {- * Text result -} text, all,
    {- * Inspecting the position -} position, withLocation,
    {- * Repetition -} repetition,
    {- * The end -} atEnd, end,
    {- * Contextualizing errors -} contextualize, (<?>),
    {- * Failure -} failure,
    -- {- * Transformation -} under, while,
  )
  where

import Step.Internal.Prelude hiding (while, under, Is)

import Step.Document.Parser
import qualified Step.Document.Parser as Parser

import qualified Loc
import Loc (Loc, SpanOrLoc)

import Step.Cursor.Base (Cursor (Cursor))
import qualified Step.Cursor.Base as Cursor

import Step.Buffer.Base (Buffer)
import qualified Step.Buffer.Base as Buffer

import Step.BufferedStream.Base (BufferedStream)
import qualified Step.BufferedStream.Base as BufferedStream

import qualified Step.Cursor.State as Cursor.State

import qualified Step.DocumentMemory.State as DocumentMemory.State

import qualified ListLike

import qualified ListT
import ListT (ListT (ListT))

import qualified Step.Tentative.State as Tentative.State

import Step.Document.Error (Error (Error))
import qualified Step.Document.Error as Error

import Step.Document.Config (Config)
import qualified Step.Document.Config as Config

import Step.Nontrivial.Base (Nontrivial)

char :: Monad m => ListLike text Char => Parser text 'Backtracking m Char
char = AnyParser \config ->
    DocumentMemory.State.takeChar <&> maybe (Left (makeError config)) Right

satisfy :: Monad m => ListLike text Char => (Char -> Bool) -> Parser text 'Backtracking m Char
satisfy ok = AnyParser \config ->
    DocumentMemory.State.takeCharIf ok <&> \case
        Nothing -> Left (makeError config)
        Just x -> Right x

text :: Monad m => ListLike text Char => Eq text => text -> Parser text 'Committing m ()
text x = AnyParser \config ->
    DocumentMemory.State.takeText x <&> \case
        True -> Right ()
        False -> Left (makeError config)

atEnd :: Monad m => ListLike text Char => Parser text 'Certainty0 m Bool
atEnd = CertainParser \_config -> DocumentMemory.State.atEnd

end :: Monad m => ListLike text Char => Parser text 'Certainty0 m ()
end = AnyParser \config ->
    DocumentMemory.State.atEnd <&> \case
        True -> Right ()
        False -> Left (makeError config)

contextualize :: Monad m => text
    -> Parser text pt m a
    -> Parser text pt m a
contextualize c = \case
    AnyParser f -> AnyParser \config -> f (g config)
    CertainParser f -> CertainParser \config -> f (g config)
  where
    g config = config & over Config.contextLens (c :)

(<?>) :: Monad m =>
       Parser text pt m a
    -> text
    -> Parser text pt m a
p <?> c = contextualize c p

position :: Monad m => ListLike text char => Parser text 'Certainty0 m Loc
position = CertainParser \_config -> DocumentMemory.State.getPosition

withLocation :: ListLike text Char => Monad m =>
       Parser text pt m a
    -> Parser text pt m (SpanOrLoc, a)
withLocation = \case
    AnyParser p -> AnyParser \config -> do
        a <- DocumentMemory.State.getPosition
        p config >>= \case
            Left e -> return (Left e)
            Right x -> do
                b <- DocumentMemory.State.getPosition
                return (Right (Loc.spanOrLocFromTo a b, x))
    CertainParser p -> CertainParser \config -> do
        a <- DocumentMemory.State.getPosition
        x <- p config
        b <- DocumentMemory.State.getPosition
        return (Loc.spanOrLocFromTo a b, x)

repetition :: Monad m =>
    ListLike list a =>
    Parser text 'Backtracking1 m a
    -> Parser text 'Certainty m list
repetition (AnyParser p) = CertainParser \config -> fix \r ->
    p config >>= \case
        Left _ -> return ListLike.empty
        Right x -> ListLike.cons x <$> r

-- | Consume the rest of the input. This is mostly useful in conjunction with 'under'.
all :: Monad m => ListLike text Char => Parser text 'Certainty m text
all = CertainParser \_config -> DocumentMemory.State.takeAll

failure :: Monad m => Parser text 'Failure m a
failure = AnyParser \config -> return (Left (makeError config))

-- under :: Monad m => ListLike text Char => Transform text m text -> Parser text m a -> Parser text m a
-- under (Transform t) (Parser p) = Parser \eo -> do
--     s <- get
--     (s', x) <- zoom ParseState.futureLens $ lift $ runStateT (p eo) _
--     _

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

-- while :: ListLike text Char => Monad m => (Char -> Bool) -> Transform text m text
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
