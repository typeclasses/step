module Step.Document.Prelude
  (
    {- * Single character result -} char, satisfy,
    {- * Text result -} text, -- all,
    {- * Inspecting the position -} position, withLocation,
    {- * Repetition -} repetition,
    {- * The end -} atEnd, end,
    {- * Contextualizing errors -} contextualize, (<?>),
    {- * Failure -} failure,
    -- {- * Transformation -} under, while,
  )
  where

import Step.Internal.Prelude hiding (while, under, Is)

import Optics

import Step.Document.Parser (action, action', Parser)
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

import qualified Step.Document.Do as P

import qualified Step.Action.Do as Action
import qualified Step.Action.UnifiedType as Action
import Step.Action.UnifiedType (IsAction, (:>), ActionJoin)
import Step.Action.SeparateTypes

char :: Monad m => ListLike text Char => Parser text MoveUndo m Char
char = review action' $ MoveUndo \config -> runStateT $
    DocumentMemory.State.takeChar <&> \case
        Nothing -> Left (Parser.makeError config)
        Just x -> Right x
        -- maybe (Left (makeError config)) Right

satisfy :: Monad m => ListLike text Char => (Char -> Bool) -> Parser text MoveUndo m Char
satisfy ok = review action' $ MoveUndo \config -> runStateT $
    DocumentMemory.State.takeCharIf ok <&> \case
        Nothing -> Left (Parser.makeError config)
        Just x -> Right x

text :: Monad m => ListLike text Char => Eq text => text -> Parser text Any m ()
text x = review action' $ Any \config -> runStateT $
    DocumentMemory.State.takeText x <&> \case
        True -> Right ()
        False -> Left (Parser.makeError config)

atEnd :: Monad m => ListLike text Char => Parser text SureStatic m Bool
atEnd = review action' $ SureStatic \_config -> runStateT DocumentMemory.State.atEnd

end :: Monad m => ListLike text Char => Parser text Static m ()
end = review action' $ Static \config -> runStateT $
    DocumentMemory.State.atEnd <&> \case
        True -> Right ()
        False -> Left (Parser.makeError config)

contextualize :: (Monad m, ConfigurableAction k, IsAction k) =>
    text -> Parser text k m a -> Parser text k m a
contextualize c = Optics.over action (Action.configure (over Config.contextLens (c :)))

(<?>) :: (Monad m, ConfigurableAction k, IsAction k) => Parser text k m a -> text -> Parser text k m a
p <?> c = contextualize c p

position :: Monad m => ListLike text char => Parser text SureStatic m Loc
position = review action' $ SureStatic \_config -> runStateT DocumentMemory.State.getPosition

withLocation ::
    ListLike text char => Monad m => IsAction k =>
    Parser text k m a -> Parser text k m (SpanOrLoc, a)
withLocation p = P.do
    a <- position
    x <- p
    b <- position
    P.return (Loc.spanOrLocFromTo a b, x)

try :: Monad m => Action.Noncommittal k => Parser text k m a -> Parser text Sure m (Maybe a)
try = review action . Action.try . view action

repetition ::
    Monad m => ListLike list a =>
    Action.AlwaysMoves k =>
    Action.Noncommittal k =>
    Parser text k m a
    -> Parser text Sure m list
repetition p = fix \r -> P.do
    xm <- try p
    case xm of
        Nothing -> return ListLike.empty
        Just x -> ListLike.cons x <$> r

failure :: Monad m => Action.CanFail k => Parser text k m a
failure = review action $ Action.failure Parser.makeError

-- -- | Consume the rest of the input. This is mostly useful in conjunction with 'under'.
-- all :: Monad m => ListLike text Char => Parser text 'Sure m text
-- all = CertainParser \_config -> DocumentMemory.State.takeAll

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
