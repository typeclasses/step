module SupplyChain.Core.Effect (Effect (Request, Perform), run, absurd,
    alterRequest, alterPerform) where

import Data.Kind (Type)

import SupplyChain.Core.Kinds (Action, Interface, NoInterface, NoAction)

data Effect (up :: Interface) (action :: Action) (product :: Type) =
    Request (up product)
  | Perform (action product)

run :: Effect NoInterface action product -> action product
run = \case
    Perform x -> x
    Request x -> \case{} x

absurd :: Effect NoInterface NoAction x -> product
absurd = \case
    Perform x -> \case{} x
    Request x -> \case{} x

alterRequest :: (up product -> Effect up' action product)
    -> Effect up action product -> Effect up' action product
alterRequest f = \case
    Perform x -> Perform x
    Request x -> f x

alterPerform :: (action product -> Effect up action' product)
    -> Effect up action product -> Effect up action' product
alterPerform f = \case
    Request x -> Request x
    Perform x -> f x
