module Step.Package.Failure where

import Step.Action.Core
import Step.Error

import qualified Step.Do as P

import Data.Bool (Bool (..))
import Data.Maybe (Maybe (..))


fail :: forall c m e a. ErrorContext e m => Failure c m e a
fail = Failure (getError @e @m)

requireTrue :: forall c m e. ErrorContext e m => Bool -> Query c m e ()
requireTrue = \case
    True -> castTo @Query (P.pure ())
    False -> castTo @Query fail

requireJust :: forall c m e a. ErrorContext e m => Maybe a -> Query c m e a
requireJust = \case
    Just x -> castTo @Query (P.pure x)
    Nothing -> castTo @Query fail
