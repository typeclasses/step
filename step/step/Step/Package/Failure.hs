module Step.Package.Failure where

import Step.Action.Core
import Step.Interface.Core

import Control.Applicative (pure)
import Data.Bool (Bool (..))
import Data.Either
import Data.Functor
import Data.Maybe (Maybe (..))

import qualified SupplyChain.Alter as Alter

class Fallible (act :: Action) where
    fail :: act c m r a

instance Fallible Any   where fail = act \r -> pure r <&> Left
instance Fallible Query where fail = act \r -> pure r <&> Left
instance Fallible Atom  where fail = Atom fail

requireTrue :: forall c m r. Bool -> Query c m r ()
requireTrue = \case
    True -> pure' ()
    False -> fail

requireJust :: forall c m r a. Maybe a -> Query c m r a
requireJust = \case
    Just x -> pure' x
    Nothing -> fail

requireAdvanceSuccess :: AdvanceResult -> Query c m r ()
requireAdvanceSuccess = \case
    AdvanceSuccess -> pure' ()
    YouCanNotAdvance{} -> fail
