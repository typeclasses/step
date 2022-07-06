{-# options_ghc -fno-warn-missing-signatures #-}

module Step.Action.Do
  (
    module Variado.Monad.Do,
    pure, return,
  )
  where

import Optics

import qualified BasePrelude
import BasePrelude (Monad)

import Step.Action.UnifiedType (Action (Action))
import Step.Action.Kinds (SureStatic)

import Variado.Monad.Do

pure :: Monad m => a -> Action SureStatic config cursor error m a
pure x = Action (BasePrelude.pure x)

return = pure
