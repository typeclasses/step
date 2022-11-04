module Step.Package.Failure where

import Step.Action.Core
import Step.Interface.Core

import qualified Step.Do as P

import Control.Applicative (pure)
import Data.Bool (Bool (..))
import Data.Maybe (Maybe (..))

import qualified SupplyChain

fail :: forall c m r a. Failure c m r r a
fail = Failure pure

requireTrue :: forall c m r. Bool -> Query c m r r ()
requireTrue = \case
    True -> castTo @Query (P.pure ())
    False -> castTo @Query fail

requireJust :: forall c m r a. Maybe a -> Query c m r r a
requireJust = \case
    Just x -> castTo @Query (P.pure x)
    Nothing -> castTo @Query fail

requireAdvanceSuccess :: AdvanceResult -> Query c m e e ()
requireAdvanceSuccess = \case
    AdvanceSuccess -> castTo @Query (P.pure ())
    YouCanNotAdvance{} -> castTo @Query fail
