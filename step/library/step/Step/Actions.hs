{-# language ConstraintKinds, FlexibleContexts, TypeFamilies, TypeOperators, ViewPatterns #-}

module Step.Actions where

import Step.Internal.Prelude

import Step.Classes.Abstract
import qualified Step.Classes.Base as C

import Step.ActionTypes.Types

import Step.ActionTypes (cast)

import qualified Step.ActionTypes.Unsafe as Action.Unsafe

import qualified Loc
import Loc (Loc, SpanOrLoc)

import Step.ActionTypes (Join)

import qualified Step.ActionTypes.Do as A

import Step.TakeOrLeave (TakeOrLeave (..))
import qualified Step.TakeOrLeave as TakeOrLeave

import qualified Monad

import qualified Text as T

import qualified Step.Nontrivial.Base as Nontrivial

import Step.LookingAhead (Prophetic, forecast)
import qualified Step.LookingAhead as LA

import Step.Advancement (Progressive, advance)

import qualified ListT

import Positive.Unsafe (Positive (PositiveUnsafe))

import Step.Nontrivial.Base (Nontrivial)
import qualified Step.Nontrivial.Base as Nontrivial
import qualified Step.Nontrivial.List as Nontrivial

import Step.ActionTypes (repetition0)

import qualified ListLike

type Cursor m = (ListLike (LA.Text m) (LA.Char m), Eq (LA.Char m), Prophetic m, Progressive m, Fallible m)

char :: Cursor m => AtomicMove m (Error m) (LA.Char m)
char = Action.Unsafe.AtomicMove $ ListT.next forecast >>= \case
    ListT.Nil -> return (Left C.failure)
    ListT.Cons x _ -> advance (PositiveUnsafe 1) $> Right (Nontrivial.head x)

peekChar :: Cursor m => Query m (Error m) (LA.Char m)
peekChar = Action.Unsafe.Query $ ListT.next forecast <&> \case
    ListT.Nil -> Left C.failure
    ListT.Cons x _ -> Right (Nontrivial.head x)

takeCharMaybe :: Cursor m => Sure m e (Maybe (LA.Char m))
takeCharMaybe = Action.Unsafe.Sure $ ListT.next forecast >>= \case
    ListT.Nil -> return Nothing
    ListT.Cons x _ -> advance (PositiveUnsafe 1) $> Just (Nontrivial.head x)

peekCharMaybe :: Cursor m => SureQuery m e (Maybe (LA.Char m))
peekCharMaybe = Action.Unsafe.SureQuery $ ListT.next forecast <&> \case
    ListT.Nil -> Nothing
    ListT.Cons x _ -> Just (Nontrivial.head x)

satisfy :: Cursor m => (LA.Char m -> Bool) -> AtomicMove m (Error m) (LA.Char m)
satisfy ok = Action.Unsafe.AtomicMove $ ListT.next forecast >>= \case
    ListT.Cons (Nontrivial.head -> x) _ | ok x -> advance (PositiveUnsafe 1) $> Right x
    _ -> return (Left C.failure)

satisfyJust :: Cursor m => (LA.Char m -> Maybe a) -> AtomicMove m (Error m) a
satisfyJust ok = Action.Unsafe.AtomicMove $ ListT.next forecast >>= \case
    ListT.Cons (ok . Nontrivial.head -> Just x) _ -> advance (PositiveUnsafe 1) $> Right x
    _ -> return (Left C.failure)

atEnd :: Cursor m => SureQuery m e Bool
atEnd = Action.Unsafe.SureQuery $ ListT.next forecast <&> \case { ListT.Nil -> True; _ -> False }

end :: Cursor m => Query m (Error m) ()
end = atEnd A.>>= guard

guard :: Cursor m => Bool -> Query m (Error m) ()
guard = \case{ True -> cast (A.return ()); False -> cast failure }

position :: Locating m => SureQuery m e Loc
position = Action.Unsafe.SureQuery C.position

withLocation ::
    Locating m =>
    Join SureQuery act =>
    Join act SureQuery =>
    act m e a -> act m e (SpanOrLoc, a)
withLocation act =
    (\a x b -> (Loc.spanOrLocFromTo a b, x))
    A.<$> position A.<*> act A.<*> position

failure :: Cursor m => Fail m (Error m) a
failure = Action.Unsafe.Fail C.failure

some :: Cursor m => AtomicMove m (Error m) (Nontrivial (LA.Text m) (LA.Char m))
some = Action.Unsafe.AtomicMove $ ListT.next forecast >>= \case
    ListT.Nil -> return (Left C.failure)
    ListT.Cons x _ -> advance (Nontrivial.length x) $> Right x

all :: Cursor m => Sure m (Error m) (LA.Text m)
all = repetition0 some <&> Nontrivial.fold

configure :: Configure m => Action.Unsafe.ChangeBase act =>
    (Config m -> Config m) -> act m e a -> act m e a
configure f = Action.Unsafe.changeBase (C.configure f)

contextualize :: Configure m => C.HasContextStack (Config m) => Action.Unsafe.ChangeBase act =>
    T.Text -> act m e a -> act m e a
contextualize n = configure (over C.contextStackLens (n :))

infix 0 <?>
(<?>) :: Configure m => C.HasContextStack (Config m) => Action.Unsafe.ChangeBase act =>
    act m e a -> T.Text -> act m e a
p <?> c = contextualize c p

-- todo: add an atomic version of 'text'

text :: Cursor m => LA.Text m -> Any m (Error m) ()
text x = case Nontrivial.refine x of
    Nothing -> return ()
    Just y -> cast (nontrivialText y)

nontrivialText :: Cursor m => Nontrivial (LA.Text m) (LA.Char m) -> Move m (Error m) ()
nontrivialText x = someOfNontrivialText x A.>>= text

-- | Returns what remainder is needed
someOfNontrivialText :: Cursor m =>
    Nontrivial (LA.Text m) (LA.Char m) -> AtomicMove m (Error m) (LA.Text m)
someOfNontrivialText x = Action.Unsafe.AtomicMove $ ListT.next forecast >>= \case
    ListT.Nil -> return (Left C.failure)
    ListT.Cons y _ ->
        if x `Nontrivial.isPrefixOf` y
        then advance (Nontrivial.length x) $> Right ListLike.empty
        else
        if y `Nontrivial.isPrefixOf` x
        then advance (Nontrivial.length y) $>
              Right
                (
                  ListLike.drop
                      (ListLike.length (Nontrivial.generalize y))
                      (Nontrivial.generalize x)
                )
        else return (Left C.failure)

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
