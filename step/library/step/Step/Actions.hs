{-# language FlexibleContexts, TypeOperators #-}

module Step.Actions where

import Step.Internal.Prelude

import Step.Classes (Peek1, Take1, Locating, Error, Fallible, Configure, Config)
import qualified Step.Classes as C

import Step.ActionTypes.Types

import Step.ActionTypes (cast, Is)

import qualified Step.ActionTypes.Unsafe as Action.Unsafe

import qualified Loc
import Loc (Loc, SpanOrLoc)

import Step.ActionTypes (Join)

import qualified Step.ActionTypes.Do as A

import Step.TakeOrLeave (TakeOrLeave (..))
import qualified Step.TakeOrLeave as TakeOrLeave

import qualified Monad

char :: (ListLike (C.Text m) char, Take1 m) => Fallible m => AtomicMove m (Error m) char
char = Action.Unsafe.AtomicMove $ C.takeCharMaybe <&> maybe (Left C.failure) Right

peekChar :: (ListLike (C.Text m) char, Peek1 m) => Fallible m => Query m (Error m) char
peekChar = Action.Unsafe.Query $ C.peekCharMaybe <&> maybe (Left C.failure) Right

considerChar :: (ListLike (C.Text m) char, Take1 m) =>
    (char -> TakeOrLeave b a) -> Sure m e (Maybe (TakeOrLeave b a))
considerChar f = Action.Unsafe.Sure $ C.considerChar f

takeCharMaybe :: (ListLike (C.Text m) char, Take1 m) => Sure m e (Maybe char)
takeCharMaybe =
    considerChar (Take . Just) <&> Monad.join . fmap TakeOrLeave.collapse

peekCharMaybe :: (ListLike (C.Text m) char, Peek1 m) => SureQuery m e (Maybe char)
peekCharMaybe = Action.Unsafe.SureQuery C.peekCharMaybe

satisfy :: (ListLike (C.Text m) char, Take1 m) => Fallible m => (char -> Bool) -> AtomicMove m (Error m) char
satisfy ok = Action.Unsafe.AtomicMove $
    C.considerChar (\x -> if ok x then Take x else Leave ())
    <&> maybe (Left C.failure) Right . Monad.join . fmap TakeOrLeave.fromTake

satisfyJust :: (ListLike (C.Text m) char, Take1 m) => Fallible m => (char -> Maybe a) -> AtomicMove m (Error m) a
satisfyJust ok = Action.Unsafe.AtomicMove $
    C.considerChar (\x -> case ok x of Just y -> Take y; Nothing -> Leave ())
    <&> maybe (Left C.failure) Right . Monad.join . fmap TakeOrLeave.fromTake

atEnd :: (ListLike (C.Text m) char, Peek1 m) => SureQuery m e Bool
atEnd = Action.Unsafe.SureQuery C.atEnd

end :: (ListLike (C.Text m) char, Peek1 m) => Fallible m => Query m (Error m) ()
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

configure :: Configure m => Action.Unsafe.ChangeBase act => (Config m -> Config m) -> act m e a -> act m e a
configure f = Action.Unsafe.changeBase (C.configure f)
