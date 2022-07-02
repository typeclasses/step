module Step.Document.Prelude
  (
    {- * Single character result -} char, satisfy,
    {- * Text result -} text, all,
    {- * Inspecting the position -} position, withLocation,
    {- * Possibility to Parser -} many, require,
    {- * The end -} atEnd, end,
    {- * Contextualizing errors -} contextualize, (<?>),
    -- {- * Failure -} failure,
    -- {- * Transformation -} under, while,
  )
  where

import Step.Internal.Prelude hiding (while, under)

import Step.Document.Parser

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

char :: Monad m => ListLike text Char => Possibility text m Char
char = Possibility DocumentMemory.State.takeChar

satisfy :: Monad m => ListLike text Char => (Char -> Bool) -> Possibility text m Char
satisfy ok = Possibility (DocumentMemory.State.takeCharIf ok)

text :: Monad m => ListLike text Char => Eq text => text -> Parser text m ()
text x = Parser \config -> do
    y <- DocumentMemory.State.takeText x
    case y of
        True -> return (Right ())
        False -> let Parser f = failure in f config

position :: Monad m => ListLike text Char => Parser text m Loc
position = Parser \_config -> Right <$> DocumentMemory.State.getPosition

atEnd :: Monad m => ListLike text Char => Parser text m Bool
atEnd = Parser \_config -> Right <$> DocumentMemory.State.atEnd

end :: Monad m => ListLike text Char => Parser text m ()
end = atEnd >>= \case True -> return (); False -> failure

failure :: Monad m => Parser text m a
failure = Parser (\config -> return (Left (Error{ Error.context = Config.context config })))

contextualize :: Monad m => text -> Parser text m a -> Parser text m a
contextualize c (Parser f) = Parser \config ->
    f (config & over Config.contextLens (c :))

(<?>) :: Monad m => Parser text m a -> text -> Parser text m a
p <?> c = contextualize c p

withLocation :: ListLike text Char => Monad m => Parser text m a -> Parser text m (SpanOrLoc, a)
withLocation p = do
    a <- position
    x <- p
    b <- position
    return (Loc.spanOrLocFromTo a b, x)

many :: Monad m => ListLike list a => Possibility text m a -> Parser text m list
many (Possibility p) = Parser \_config -> Right <$> fix \r ->
    p >>= \case
        Nothing -> return ListLike.empty
        Just x -> ListLike.cons x <$> r

require :: Monad m => Possibility text m a -> Parser text m a
require (Possibility p) = Parser \config -> do
    result <- p
    case result of
        Nothing -> let Parser f = failure in f config
        Just x -> return (Right x)

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
