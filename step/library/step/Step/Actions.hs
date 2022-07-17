{-# language FlexibleContexts, TypeOperators #-}

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

char :: (ListLike (C.Text m) char, Char1 m) => Fallible m => AtomicMove m (Error m) char
char = Action.Unsafe.AtomicMove $ C.takeCharMaybe <&> maybe (Left C.failure) Right

peekChar :: (ListLike (C.Text m) char, Char1 m) => Fallible m => Query m (Error m) char
peekChar = Action.Unsafe.Query $ C.peekCharMaybe <&> maybe (Left C.failure) Right

considerChar :: (ListLike (C.Text m) char, Char1 m) =>
    (char -> TakeOrLeave b a) -> Sure m e (Maybe (TakeOrLeave b a))
considerChar f = Action.Unsafe.Sure $ C.considerChar f

takeCharMaybe :: (ListLike (C.Text m) char, Char1 m) => Sure m e (Maybe char)
takeCharMaybe =
    considerChar (Take . Just) <&> Monad.join . fmap TakeOrLeave.collapse

peekCharMaybe :: (ListLike (C.Text m) char, Char1 m) => SureQuery m e (Maybe char)
peekCharMaybe = Action.Unsafe.SureQuery C.peekCharMaybe

satisfy :: (ListLike (C.Text m) char, Char1 m) => Fallible m => (char -> Bool) -> AtomicMove m (Error m) char
satisfy ok = Action.Unsafe.AtomicMove $
    C.considerChar (\x -> if ok x then Take x else Leave ())
    <&> maybe (Left C.failure) Right . Monad.join . fmap TakeOrLeave.fromTake

satisfyJust :: (ListLike (C.Text m) char, Char1 m) => Fallible m => (char -> Maybe a) -> AtomicMove m (Error m) a
satisfyJust ok = Action.Unsafe.AtomicMove $
    C.considerChar (\x -> case ok x of Just y -> Take y; Nothing -> Leave ())
    <&> maybe (Left C.failure) Right . Monad.join . fmap TakeOrLeave.fromTake

atEnd :: (ListLike (C.Text m) char, Char1 m) => SureQuery m e Bool
atEnd = Action.Unsafe.SureQuery C.atEnd

end :: (ListLike (C.Text m) char, Char1 m) => Fallible m => Query m (Error m) ()
end = atEnd A.>>= guard

guard :: Fallible m => Bool -> Query m (Error m) ()
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

failure :: Fallible m => Fail m (Error m) a
failure = Action.Unsafe.Fail C.failure

all :: C.TakeAll m => ListLike (C.Text m) char => Sure m (Error m) (C.Text m)
all = Action.Unsafe.Sure C.takeAll

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

text :: C.SkipTextNonAtomic m => Fallible m => ListLike (C.Text m) char => Eq char =>
    C.Text m -> Any m (Error m) ()
text x = Action.Unsafe.Any $
    C.skipTextNonAtomic x <&> \case{ True -> Right (); False -> Left C.failure }

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
