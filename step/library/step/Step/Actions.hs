{-# language FlexibleContexts #-}

module Step.Actions where

import Step.Internal.Prelude

import Step.Classes (Peek1, PeekChar, Take1, Locating, TakeChar, Error, Fallible)
import qualified Step.Classes as C

import Step.ActionTypes.Types

import qualified Step.ActionTypes.Unsafe as Action.Unsafe

import qualified Loc
import Loc (Loc, SpanOrLoc)

import Step.ActionTypes (Join)

import qualified Step.ActionTypes.Do as A

import Step.TakeOrLeave (TakeOrLeave (..))
import qualified Step.TakeOrLeave as TakeOrLeave

import qualified Monad

char :: TakeChar m char => Fallible m => AtomicMove m (Error m) char
char = Action.Unsafe.AtomicMove $ C.takeCharMaybe <&> maybe (Left C.failure) Right

peekChar :: PeekChar m char => Fallible m => Query m (Error m) char
peekChar = Action.Unsafe.Query $ C.peekCharMaybe <&> maybe (Left C.failure) Right

considerChar :: TakeChar m char =>
    (char -> TakeOrLeave b a) -> Sure m e (Maybe (TakeOrLeave b a))
considerChar f = Action.Unsafe.Sure $ C.considerChar f

takeCharMaybe :: TakeChar m char => Sure m e (Maybe char)
takeCharMaybe =
    considerChar (Take . Just) <&> Monad.join . fmap TakeOrLeave.collapse

peekCharMaybe :: PeekChar m char => SureQuery m e (Maybe char)
peekCharMaybe = Action.Unsafe.SureQuery C.peekCharMaybe

atEnd :: PeekChar m char => SureQuery m e Bool
atEnd = Action.Unsafe.SureQuery C.atEnd

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
