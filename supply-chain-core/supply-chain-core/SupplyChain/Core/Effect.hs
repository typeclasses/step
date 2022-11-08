-- | Description: an /effect/ is either /request/ or /perform/

module SupplyChain.Core.Effect
  (
    {- * Type -} Effect (Request, Perform),
    {- * Running -} run, absurd,
    {- * Alteration -} alterRequest, alterPerform,
  )
  where

import Data.Functor.Const (Const)
import Data.Void (Void)

data Effect up action product =
    Request (up product) | Perform (action product)

run :: Effect (Const Void) action product -- ^ An effect that makes no requests
    -> action product
run = \case
    Perform x -> x
    Request x -> case x of {}

absurd ::
    Effect (Const Void) (Const Void) x -- ^ There are values of this type.
    -> product
absurd = \case
    Perform x -> case x of {}
    Request x -> case x of {}

alterRequest ::
    (up product -> Effect up' action product) -- ^ Modification to requests
    -> Effect up action product -> Effect up' action product
alterRequest f = \case
    Perform x -> Perform x
    Request x -> f x

alterPerform ::
    (action product -> Effect up action' product) -- ^ Modification to actions
    -> Effect up action product -> Effect up action' product
alterPerform f = \case
    Request x -> Request x
    Perform x -> f x
