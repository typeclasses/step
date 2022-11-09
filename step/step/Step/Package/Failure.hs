module Step.Package.Failure where

import Step.Action.Core
import Step.Interface.Core

import Control.Applicative (pure)
import Data.Bool (Bool (..))
import Data.Maybe (Maybe (..))

fail :: forall c m r a. Failure c m r r a
fail = Failure pure

fail' :: forall p c m r a. Is Failure p => p c m r r a
fail' = cast fail

requireTrue :: forall c m r. Bool -> Query c m r r ()
requireTrue = \case
    True -> pure' ()
    False -> fail'

requireJust :: forall c m r a. Maybe a -> Query c m r r a
requireJust = \case
    Just x -> pure' x
    Nothing -> fail'

requireAdvanceSuccess :: AdvanceResult -> Query c m e e ()
requireAdvanceSuccess = \case
    AdvanceSuccess -> pure' ()
    YouCanNotAdvance{} -> fail'
