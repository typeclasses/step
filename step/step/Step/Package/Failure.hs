module Step.Package.Failure where

import Step.Action.Core
import Step.Interface.Core

import Control.Applicative (pure)
import Data.Bool (Bool (..))
import Data.Maybe (Maybe (..))
import Data.Void
import Data.Functor.Const
import SupplyChain
import qualified SupplyChain.Alter as Alter
import Data.Function
import Data.Functor
import Data.Either

class CanFail (act :: Action) where
    fail :: (r -> Job (Const Void) m e) -> act c m r e a

instance CanFail Any where fail x = act \r -> Alter.job' (Alter.request' \case{}) (x r) <&> Left
instance CanFail Query where fail x = act \r -> Alter.job' (Alter.request' \case{}) (x r) <&> Left
instance CanFail Atom where fail x = Atom $ act \r -> Alter.job' (Alter.request' \case{}) (x r) <&> Left

requireTrue :: forall c m r. Bool -> Query c m r r ()
requireTrue = \case
    True -> pure' ()
    False -> fail pure

requireJust :: forall c m r a. Maybe a -> Query c m r r a
requireJust = \case
    Just x -> pure' x
    Nothing -> fail pure

requireAdvanceSuccess :: AdvanceResult -> Query c m e e ()
requireAdvanceSuccess = \case
    AdvanceSuccess -> pure' ()
    YouCanNotAdvance{} -> fail pure
