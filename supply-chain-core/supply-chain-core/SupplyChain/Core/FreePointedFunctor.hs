-- | Description: makes a pointed functor of any type constructor

module SupplyChain.Core.FreePointedFunctor
  (
    {- * Type -} FreePointedFunctor (Pure, Map),
    {- * Running -} run, eval,
    {- * Alteration -} alter,
  )
  where

import Control.Applicative (pure)
import Control.Monad (Monad)
import Data.Function ((&))
import Data.Functor (Functor, (<&>))

data FreePointedFunctor f product =
    Pure product
  | forall x. Map (f x) (x -> product)

deriving instance Functor (FreePointedFunctor f)

run :: Monad effect =>
    (forall x. f x -> effect x) -- ^ How to interpret @f@ actions
    -> FreePointedFunctor f product -> effect product
run runEffect = \case
    Pure product -> pure product
    Map action extract -> runEffect action <&> extract

eval ::
    (forall x. f x -> x) -- ^ How to interpret @f@ actions
    -> FreePointedFunctor f product -> product
eval evalF = \case
    Pure product -> product
    Map action extract -> evalF action & extract

alter :: (forall x. f x -> FreePointedFunctor f' x)
    -> FreePointedFunctor f product -> FreePointedFunctor f' product
alter f = \case
    Pure product -> Pure product
    Map action extract -> f action <&> extract
