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

import qualified Step.Action.UnifiedType as U
import qualified Step.Action.SeparateTypes as S

import Variado.Monad.Do

pure :: Monad m => a -> U.Action S.SureStatic config cursor error m a
pure x = view U.actionIso (BasePrelude.pure x)

return = pure
