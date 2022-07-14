{-# language FlexibleContexts #-}

module Step.Actions where

import Step.Internal.Prelude

import Step.Classes (Peek1, PeekChar, Take1, Locating)
import qualified Step.Classes as C

import Step.ActionTypes.Types

import qualified Step.ActionTypes.Unsafe as Action.Unsafe

import qualified Loc
import Loc (Loc, SpanOrLoc)

import Step.ActionTypes (Join)

import qualified Step.ActionTypes.Do as A

takeCharMaybe :: PeekChar m char => Take1 m => (char -> Maybe a) -> SureQuery m e (Maybe a)
takeCharMaybe f = Action.Unsafe.SureQuery $ C.takeCharMaybe f

next :: PeekChar m char => SureQuery m e (Maybe char)
next = Action.Unsafe.SureQuery C.next

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
