module SupplyChain.Core.Effect (Effect (Request, Perform), run, absurd,
    alterRequest, alterPerform) where

import Data.Functor.Const (Const)
import Data.Void (Void)

data Effect up action product =
    Request (up product) | Perform (action product)

run :: Effect (Const Void) action product -> action product
run = \case
    Perform x -> x
    Request x -> case x of {}

absurd :: Effect (Const Void) (Const Void) x -> product
absurd = \case
    Perform x -> case x of {}
    Request x -> case x of {}

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
