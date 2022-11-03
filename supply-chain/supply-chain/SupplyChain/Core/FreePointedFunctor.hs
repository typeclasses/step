module SupplyChain.Core.FreePointedFunctor
    (T (Pure, Map), run) where

import Control.Monad (Monad)
import Data.Functor (Functor, (<&>))

import SupplyChain.Core.RunnerType (type (->>))

data T con product =
    Pure product
  | forall x. Map (con x) (x -> product)

deriving stock instance Functor (T con)

run :: Monad effect =>
    (product -> effect product) -> (con ->> effect)
    -> T con product -> effect product

run runPure runEffect =
  \case
    Pure product        ->  runPure product
    Map action extract  ->  runEffect action <&> extract
