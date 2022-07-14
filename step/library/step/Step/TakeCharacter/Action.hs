{-# language FlexibleContexts #-}

module Step.TakeCharacter.Action where

import Step.Internal.Prelude

import Step.TakeCharacter.Class (TakeCharacter)
import qualified Step.TakeCharacter.Class as TakeCharacter

import Step.LookAhead.Class (LookAhead)
import qualified Step.LookAhead.Class as LookAhead

import Step.ActionTypes
import qualified Step.ActionTypes.Unsafe as Action.Unsafe

takeCharMaybe :: LookAhead.Char m char => TakeCharacter m => (char -> Maybe a) -> SureQuery m e (Maybe a)
takeCharMaybe f = Action.Unsafe.SureQuery $ TakeCharacter.takeCharMaybe f
