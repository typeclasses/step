module SupplyChain.Core.FreeMonad
    (T (Step, Bind, Pure, Map), run) where

import Data.Functor (Functor)
import Control.Applicative (Applicative (pure, (<*>)))
import Control.Monad (Monad ((>>=)))
import qualified Control.Monad as Monad

import SupplyChain.Core.RunnerType (type (->>))
import qualified SupplyChain.Core.FreePointedFunctor as PointedFunctor

data T con product =
    Step (PointedFunctor.T con product)
  | forall x. Bind (T con x) (x -> T con product)

pattern Pure :: product -> T con product
pattern Pure product = Step (PointedFunctor.Pure product)

pattern Map :: con x -> (x -> product) -> T con product
pattern Map action extract = Step (PointedFunctor.Map action extract)

{-# complete Pure, Map, Bind #-}

deriving stock instance Functor (T con)

instance Applicative (T con)
  where
    pure = Pure
    (<*>) = Monad.ap

instance Monad (T con)
  where
    (>>=) = Bind

run :: forall effect con product. Monad effect =>
    (con ->> effect) -> T con product -> effect product

run runEffect = go
  where
    runPF :: PointedFunctor.T con x -> effect x
    runPF = PointedFunctor.run pure runEffect

    go :: T con x -> effect x
    go = \case
        Step a -> runPF a
        Bind (Step a) b -> runPF a >>= \x -> go (b x)
        Bind (Bind a b) c -> go (Bind a \x -> Bind (b x) c)
