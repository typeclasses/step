{-# language FlexibleContexts, QualifiedDo, TypeFamilies #-}

module Step.Document.Prelude
  (
    {- * Single character result -} char, satisfy, satisfyJust, peekChar, peekCharMaybe,
    {- * Text result -} text, all,
    {- * Inspecting the position -} position, withLocation,
    {- * The end -} atEnd, end,
    {- * Contextualizing errors -} contextualize, (<?>),
    {- * Failure -} failure,
    {- * Transformation -} -- within,
    {- * Extent -} -- while,
  )
  where

import Step.Internal.Prelude hiding (while, under)

import Optics

import qualified NonEmpty

import qualified Step.Document.Parser as Parser

import qualified Loc
import Loc (Loc, SpanOrLoc)

import Step.DocumentMemory.Base (DocumentMemory)
import qualified Step.DocumentMemory.State as DocumentMemory.State

import qualified Step.Document.Config as Config

import qualified Step.ActionTypes.Do as P

import Step.ActionTypes (Atomic, Loop0, Loop1)
import qualified Step.ActionTypes as Action
import Step.ActionTypes.Types

import qualified Step.ActionTypes.Unsafe as Action.Unsafe

import Step.Extent.BufferedStream (Extent (Extent))

import Step.Document.Error (Error)

import Step.Document.Config (Config)

import Step.LookAhead.Class (LookAhead)
import qualified Step.LookAhead.Class as LookAhead

char :: Monad base => ListLike text char =>
    AtomicMove Error (ReaderT Config (StateT (DocumentMemory text base) base)) char
char = Action.Unsafe.AtomicMove $ ReaderT \c ->
    DocumentMemory.State.takeChar <&> \case
        Nothing -> Left (Parser.makeError c)
        Just x -> Right x

peekChar :: LookAhead base text => ListLike text char =>
    Query Error (ReaderT Config base) char
peekChar = Action.Unsafe.Query $ ReaderT \c ->
    LookAhead.peekCharMaybe <&> \case
        Nothing -> Left (Parser.makeError c)
        Just x -> Right x

peekCharMaybe :: LookAhead base text => ListLike text char =>
    SureQuery Error (ReaderT Config base) (Maybe char)
peekCharMaybe = Action.Unsafe.SureQuery $ ReaderT \_ -> LookAhead.peekCharMaybe

satisfy :: Monad base => ListLike text char => (char -> Bool)
    -> AtomicMove Error (ReaderT Config (StateT (DocumentMemory text base) base)) char
satisfy ok = Action.Unsafe.AtomicMove $ ReaderT \c ->
    DocumentMemory.State.takeCharIf ok <&> \case
        Nothing -> Left (Parser.makeError c)
        Just x -> Right x

satisfyJust :: Monad base => ListLike text char => (char -> Maybe a)
    -> AtomicMove Error (ReaderT Config (StateT (DocumentMemory text base) base)) a
satisfyJust ok = Action.Unsafe.AtomicMove $ ReaderT \c ->
    DocumentMemory.State.takeCharJust ok <&> \case
        Nothing -> Left (Parser.makeError c)
        Just x -> Right x

-- todo: add an atomic version of 'text'

text :: Monad base => ListLike text char => Eq text => Eq char => text
    -> Any Error (ReaderT Config (StateT (DocumentMemory text base) base)) ()
text x = Action.Unsafe.Any $ ReaderT \c ->
    DocumentMemory.State.takeTextNotAtomic x <&> \case
        True -> Right ()
        False -> Left (Parser.makeError c)

atEnd :: Monad base => ListLike text char =>
    SureQuery Error (ReaderT Config (StateT (DocumentMemory text base) base)) Bool
atEnd = Action.Unsafe.SureQuery $ ReaderT \_ -> DocumentMemory.State.atEnd

end :: Monad base => ListLike text char =>
    Query Error (ReaderT Config (StateT (DocumentMemory text base) base)) ()
end = Action.Unsafe.Query $ ReaderT \c ->
    DocumentMemory.State.atEnd <&> \case
        True -> Right ()
        False -> Left (Parser.makeError c)

contextualize :: Monad base => Action.Unsafe.ChangeBase k => Text
    -> k Error (ReaderT Config (StateT (DocumentMemory text base) base)) a
    -> k Error (ReaderT Config (StateT (DocumentMemory text base) base)) a
contextualize n = Action.Unsafe.changeBase (withReaderT (over Config.contextLens (n :)))

infix 0 <?>
(<?>) :: Monad base => Action.Unsafe.ChangeBase k =>
    k Error (ReaderT Config (StateT (DocumentMemory text base) base)) a
    -> Text
    -> k Error (ReaderT Config (StateT (DocumentMemory text base) base)) a
p <?> c = contextualize c p

position :: Monad base => ListLike text char =>
    SureQuery Error (ReaderT Config (StateT (DocumentMemory text base) base)) Loc
position = Action.Unsafe.SureQuery $ ReaderT \_ -> DocumentMemory.State.getPosition

withLocation ::
    ListLike text char => Monad base =>
    Action.Join SureQuery k =>
    Action.Join k SureQuery =>
    k Error (ReaderT Config (StateT (DocumentMemory text base) base)) a
    -> k Error (ReaderT Config (StateT (DocumentMemory text base) base)) (SpanOrLoc, a)
withLocation p =
    (\a x b -> (Loc.spanOrLocFromTo a b, x)) P.<$> position P.<*> p P.<*> position

failure :: Monad base =>
    Fail Error (ReaderT Config (StateT (DocumentMemory text base) base)) a
failure = Action.Unsafe.Fail $ ReaderT Parser.makeError

-- -- | Consume the rest of the input. This is mostly useful in conjunction with 'within'.
all :: Monad base => ListLike text char =>
    Sure Error (ReaderT Config (StateT (DocumentMemory text base) base)) text
all = Action.Unsafe.Sure $ ReaderT \_ -> DocumentMemory.State.takeAll

-- within :: Monad m => ListLike text char => Action.Unsafe.CoerceAny k =>
--     Extent (StateT (DocumentMemory text m) m) text
--     -> Parser text m k a
--     -> Parser text m k a
-- within e = over o (DocumentMemory.State.within e)
--   where
--     o =
--         iso (\(Parser p) -> p) Parser
--         % iso (\(ActionReader f) -> f) ActionReader
--         % mapped
--         % Action.Unsafe.anyIsoUnsafe
--         % iso (\(Action.Unsafe.Any s) -> s) Action.Unsafe.Any

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
