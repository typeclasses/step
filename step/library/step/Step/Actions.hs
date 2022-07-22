{-# language ConstraintKinds, FlexibleContexts, NamedFieldPuns, TypeFamilies, TypeOperators, ViewPatterns #-}

module Step.Actions where

import Step.Internal.Prelude

import Step.ActionTypes.Types

import Step.ActionTypes (cast)

import qualified Step.ActionTypes.Unsafe as Action.Unsafe

import qualified Loc
import Loc (Loc, SpanOrLoc)

import Step.ActionTypes (Join)

import qualified Step.ActionTypes.Do as A

import qualified Text as T

import qualified Step.Nontrivial.Base as Nontrivial

import Step.Input.Cursor (Text, Char, curse, Session (..))
import qualified Step.Input.Cursor as Cursor

import Positive.Unsafe (Positive (PositiveUnsafe))

import Step.Nontrivial.Base (Nontrivial)
import qualified Step.Nontrivial.List as Nontrivial

import Step.ActionTypes (repetition0)

import qualified ListLike

import Step.Document.Locating (Locating)
import qualified Step.Document.Locating as Locating

import Step.Failure (Fallible, Error)
import qualified Step.Failure as F

import Step.Configuration (Configure, HasContextStack, contextStackLens, Config)
import qualified Step.Configuration as Config

import Step.Input.Counter (Counting)
import qualified Step.Input.Counter as Counting

import Step.Input.CursorPosition (CursorPosition)

type Cursor m = (ListLike (Text m) (Char m), Eq (Char m), Cursor.Cursor m, Fallible m)

char :: Cursor m => AtomicMove m (Error m) (Char m)
char = Action.Unsafe.AtomicMove $ case curse of
    Session{ run, next, commit } -> run $ next >>= \case
        Nothing -> return (Left F.failure)
        Just x -> commit (PositiveUnsafe 1) $> Right (Nontrivial.head x)

peekChar :: Cursor m => Query m (Error m) (Char m)
peekChar = Action.Unsafe.Query $ case curse of
    Session{ run, next } -> run $ next <&> \case
        Nothing -> Left F.failure
        Just x -> Right (Nontrivial.head x)

takeCharMaybe :: Cursor m => Sure m e (Maybe (Char m))
takeCharMaybe = Action.Unsafe.Sure $ case curse of
    Session{ run, next, commit } -> run $ next >>= \case
        Nothing -> return Nothing
        Just x -> commit (PositiveUnsafe 1) $> Just (Nontrivial.head x)

peekCharMaybe :: Cursor m => SureQuery m e (Maybe (Char m))
peekCharMaybe = Action.Unsafe.SureQuery $ case curse of
    Session{ run, next } -> run $ next <&> \case
        Nothing -> Nothing
        Just x -> Just (Nontrivial.head x)

satisfy :: Cursor m => (Char m -> Bool) -> AtomicMove m (Error m) (Char m)
satisfy ok = Action.Unsafe.AtomicMove $ case curse of
    Session{ run, next, commit } -> run $ next >>= \case
        Just (Nontrivial.head -> x) | ok x -> commit (PositiveUnsafe 1) $> Right x
        _ -> return (Left F.failure)

satisfyJust :: Cursor m => (Char m -> Maybe a) -> AtomicMove m (Error m) a
satisfyJust ok = Action.Unsafe.AtomicMove $ case curse of
    Session{ run, next, commit } -> run $ next >>= \case
        Just (ok . Nontrivial.head -> Just x) -> commit (PositiveUnsafe 1) $> Right x
        _ -> return (Left F.failure)

atEnd :: Cursor m => SureQuery m e Bool
atEnd = Action.Unsafe.SureQuery $ case curse of
    Session{ run, next } -> run $ next <&> isNothing

end :: Cursor m => Query m (Error m) ()
end = atEnd A.>>= guard

guard :: Cursor m => Bool -> Query m (Error m) ()
guard = \case{ True -> cast (A.return ()); False -> cast failure }

cursorPosition :: Counting m => SureQuery m e CursorPosition
cursorPosition = Action.Unsafe.SureQuery Counting.cursorPosition

position :: Locating m => SureQuery m e Loc
position = Action.Unsafe.SureQuery Locating.position

withLocation ::
    (Monad m, Locating m) =>
    Join SureQuery act =>
    Join act SureQuery =>
    act m e a -> act m e (SpanOrLoc, a)
withLocation act =
    (\a x b -> (Loc.spanOrLocFromTo a b, x))
    A.<$> position A.<*> act A.<*> position

failure :: Cursor m => Fail m (Error m) a
failure = Action.Unsafe.Fail F.failure

some :: Cursor m => AtomicMove m (Error m) (Nontrivial (Text m) (Char m))
some = Action.Unsafe.AtomicMove $ case curse of
    Session{ run, next, commit } -> run $ next >>= \case
        Nothing -> return (Left F.failure)
        Just x -> commit (Nontrivial.length x) $> Right x

all :: Cursor m => Sure m (Error m) (Text m)
all = repetition0 some <&> Nontrivial.fold

configure :: Configure m => Action.Unsafe.ChangeBase act =>
    (Config m -> Config m) -> act m e a -> act m e a
configure f = Action.Unsafe.changeBase (Config.configure f)

contextualize :: Configure m => HasContextStack (Config m) => Action.Unsafe.ChangeBase act =>
    T.Text -> act m e a -> act m e a
contextualize n = configure (over contextStackLens (n :))

infix 0 <?>
(<?>) :: Configure m => HasContextStack (Config m) => Action.Unsafe.ChangeBase act =>
    act m e a -> T.Text -> act m e a
p <?> c = contextualize c p

-- todo: add an atomic version of 'text'

text :: Cursor m => Text m -> Any m (Error m) ()
text x = case Nontrivial.refine x of
    Nothing -> return ()
    Just y -> cast (nontrivialText y)

nontrivialText :: Cursor m => Nontrivial (Text m) (Char m) -> Move m (Error m) ()
nontrivialText x = someOfNontrivialText x A.>>= text

-- | Returns what remainder is needed
someOfNontrivialText :: Cursor m =>
    Nontrivial (Text m) (Char m) -> AtomicMove m (Error m) (Text m)
someOfNontrivialText x = Action.Unsafe.AtomicMove $ case curse of
    Session{ run, next, commit } -> run $ next >>= \case
        Nothing -> return (Left F.failure)
        Just y ->
            if x `Nontrivial.isPrefixOf` y
            then commit (Nontrivial.length x) $> Right ListLike.empty
            else
            if y `Nontrivial.isPrefixOf` x
            then commit (Nontrivial.length y) $>
                  Right
                    (
                      ListLike.drop
                          (ListLike.length (Nontrivial.generalize y))
                          (Nontrivial.generalize x)
                    )
            else return (Left F.failure)

-- while ::
--     Action.Unsafe.ChangeBase act =>
--     ListLike (C.Text m) char =>
--     C.While m =>
--     (char -> Bool)
--     -> act m (Error m) (C.Text m)
--     -> act m (Error m) (C.Text m)
-- while ok = Action.Unsafe.changeBase (C.while ok)

-- within :: Monad m => ListLike text char => Action.Unsafe.CoerceAny act =>
--     Extent (StateT (DocumentMemory text m) m) text
--     -> Parser text m act a
--     -> Parser text m act a
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
