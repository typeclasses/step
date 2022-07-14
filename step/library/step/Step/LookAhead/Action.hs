{-# language FlexibleContexts #-}

module Step.LookAhead.Action where

import Step.Internal.Prelude

import Step.LookAhead.Class (LookAhead, Char)
import qualified Step.LookAhead.Class as LookAhead

import Step.ActionTypes
import qualified Step.ActionTypes.Unsafe as Action.Unsafe

next :: LookAhead base => Char base char => SureQuery e base (Maybe char)
next = Action.Unsafe.SureQuery LookAhead.next

atEnd :: LookAhead base => Char base char => SureQuery e base Bool
atEnd = Action.Unsafe.SureQuery LookAhead.atEnd
