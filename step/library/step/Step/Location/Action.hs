{-# language FlexibleContexts #-}

module Step.Location.Action where

import Step.Internal.Prelude

import Step.ActionTypes.Types

import qualified Step.ActionTypes.Unsafe as Action.Unsafe

import qualified Loc
import Loc (Loc, SpanOrLoc)

import qualified Step.Location.Class as Locating
import Step.Location.Class (Locating)

import Step.ActionTypes (Join)

import qualified Step.ActionTypes.Do as A

position :: Locating m => SureQuery m e Loc
position = Action.Unsafe.SureQuery Locating.position

withLocation ::
    Locating m =>
    Join SureQuery act =>
    Join act SureQuery =>
    act m e a -> act m e (SpanOrLoc, a)
withLocation act =
    (\a x b -> (Loc.spanOrLocFromTo a b, x))
    A.<$> position A.<*> act A.<*> position
