module SupplyChain.Core.FreePointedFunctor (FreePointedFunctor (Pure, Map), run,
eval, alter) where

import Control.Monad (Monad)
import Data.Functor (Functor, (<&>))
import Data.Function ((&))

data FreePointedFunctor con product =
    Pure product
  | forall x. Map (con x) (x -> product)

deriving instance Functor (FreePointedFunctor con)

run :: Monad effect => (product -> effect product)
    -> (forall x. con x -> effect x)
    -> FreePointedFunctor con product -> effect product
run runPure runEffect = \case
    Pure product -> runPure product
    Map action extract -> runEffect action <&> extract

eval :: (forall x. con x -> x) -> FreePointedFunctor con product -> product
eval evalCon = \case
    Pure product -> product
    Map action extract -> evalCon action & extract

alter :: (forall x. con x -> FreePointedFunctor con' x)
    -> FreePointedFunctor con product -> FreePointedFunctor con' product
alter f = \case
    Pure product -> Pure product
    Map action extract -> f action <&> extract
