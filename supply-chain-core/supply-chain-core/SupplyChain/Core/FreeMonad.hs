module SupplyChain.Core.FreeMonad (FreeMonad (Step, Bind, Pure, Map), run, eval,
alter) where

import Data.Functor (Functor, (<&>))
import Data.Function ((&), ($), (.))
import Control.Applicative (Applicative (pure, (<*>)))
import Control.Monad (Monad ((>>=)))
import qualified Control.Monad as Monad

import SupplyChain.Core.FreePointedFunctor (FreePointedFunctor)
import qualified SupplyChain.Core.FreePointedFunctor as FreePointedFunctor

data FreeMonad con product =
    Step (FreePointedFunctor con product)
  | forall x. Bind (FreeMonad con x) (x -> FreeMonad con product)

pattern Pure :: product -> FreeMonad con product
pattern Pure product = Step (FreePointedFunctor.Pure product)

pattern Map :: con x -> (x -> product) -> FreeMonad con product
pattern Map action extract = Step (FreePointedFunctor.Map action extract)

{-# complete Pure, Map, Bind #-}

deriving instance Functor (FreeMonad con)

instance Applicative (FreeMonad con) where pure = Pure; (<*>) = Monad.ap

instance Monad (FreeMonad con) where (>>=) = Bind

run :: Monad effect => (forall x. con x -> effect x)
    -> FreeMonad con product -> effect product
run (runEffect :: forall x. con x -> effect x) = recur
  where
    runPF :: FreePointedFunctor con x -> effect x
    runPF = FreePointedFunctor.run pure runEffect

    recur :: FreeMonad con x -> effect x
    recur = \case
        Step a -> runPF a
        Bind (Step a) b -> runPF a >>= \x -> recur $ b x
        Bind (Bind a b) c -> recur a >>= \x -> recur $ Bind (b x) c

eval :: (forall x. con x -> x)
    -> FreeMonad con product -> product
eval (evalEffect :: forall x. con x -> x) = recur
  where
    evalPF :: FreePointedFunctor con x -> x
    evalPF = FreePointedFunctor.eval evalEffect

    recur :: FreeMonad con x -> x
    recur = \case
        Step a -> evalPF a
        Bind (Step a) b -> evalPF a & \x -> recur $ b x
        Bind (Bind a b) c -> recur a & \x -> recur $ Bind (b x) c

alter :: (forall x. con x -> FreeMonad con' x)
    -> FreeMonad con product -> FreeMonad con' product
alter (f :: forall x. con x -> FreeMonad con' x) = recur
  where
    recur :: FreeMonad con x -> FreeMonad con' x
    recur = \case
        Pure x -> Pure x
        Map action extract -> f action <&> extract
        Bind a b -> Bind (recur a) (recur . b)
