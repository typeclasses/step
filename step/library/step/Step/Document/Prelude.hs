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

import Step.Internal.Prelude hiding (while, under)

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

char :: forall m text p. Monad m => ListLike text Char => Lift Possibility1 p => p text m Char
char = Parser.lift $ Possibility1 \config ->
    DocumentMemory.State.takeChar <&> maybe (Left (makeError config)) Right

satisfy :: forall m text p. Monad m => ListLike text Char => (Char -> Bool) -> Lift Possibility1 p => p text m Char
satisfy ok = Parser.lift $ Possibility1 \config ->
    DocumentMemory.State.takeCharIf ok <&> maybe (Left (makeError config)) Right

text :: Monad m => ListLike text Char => Eq text => text -> Parser text m ()
text x = Parser \config ->
    DocumentMemory.State.takeText x <&> \case
        True -> Right ()
        False -> Left (makeError config)

position :: forall m text p. Monad m => ListLike text Char => Lift Always p => p text m Loc
position = Parser.lift $ Always \_config -> DocumentMemory.State.getPosition

atEnd :: forall m text p. Monad m => ListLike text Char => Lift Always p => p text m Bool
atEnd = Parser.lift $ Always \_config -> DocumentMemory.State.atEnd

end :: forall m text p. Monad m => ListLike text Char => Lift Possibility p => p text m ()
end = Parser.lift $ Possibility \config ->
    DocumentMemory.State.atEnd <&> \case True -> Right (); False -> Left (makeError config)

failure :: Monad m => Lift Failure p => p text m a
failure = Parser.lift Failure

contextualize :: Monad m => text -> Parser text m a -> Parser text m a
contextualize c (Parser f) = Parser \config ->
    f (config & over Config.contextLens (c :))

(<?>) :: Monad m => Parser text m a -> text -> Parser text m a
p <?> c = contextualize c p

withLocation :: forall m text a. ListLike text Char => Monad m => Parser text m a -> Parser text m (SpanOrLoc, a)
withLocation p = do
    a <- position
    x <- p
    b <- position
    return (Loc.spanOrLocFromTo a b, x)

repetition :: forall m text list a p'. Monad m => ListLike list a => Lift Always p' => Possibility1 text m a -> p' text m list
repetition (Possibility1 p) = Parser.lift $ Always \config -> fix \r ->
    p config >>= \case
        Left _ -> return ListLike.empty
        Right x -> ListLike.cons x <$> r

-- | Consume the rest of the input. This is mostly useful in conjunction with 'under'.
all :: Monad m => ListLike text Char => Parser text m text
all = Parser \_config -> Right <$> DocumentMemory.State.takeAll

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
