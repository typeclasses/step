{-# language FlexibleContexts #-}

module Step.LookAhead.Action where

import Step.Internal.Prelude

import Step.LookAhead.Class (LookAhead, Char)
import qualified Step.LookAhead.Class as LookAhead

import Step.ActionTypes
import qualified Step.ActionTypes.Unsafe as Action.Unsafe

next :: Char m char => SureQuery m e (Maybe char)
next = Action.Unsafe.SureQuery LookAhead.next

atEnd :: Char m char => SureQuery m e Bool
atEnd = Action.Unsafe.SureQuery LookAhead.atEnd
