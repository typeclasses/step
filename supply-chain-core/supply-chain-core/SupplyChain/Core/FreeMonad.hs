-- | Description: makes a monad of any type constructor

module SupplyChain.Core.FreeMonad
  (
    {- * Type -} FreeMonad (Step, Bind, Pure, Map),
    {- * Running -} run, eval,
    {- * Alteration -} alter,
  )
  where

import Control.Applicative (Applicative (pure, (<*>)))
import Control.Monad (Monad ((>>=)))
import Data.Function ((&), ($), (.))
import Data.Functor (Functor, (<&>))
import SupplyChain.Core.FreePointedFunctor (FreePointedFunctor)

import qualified Control.Monad as Monad
import qualified SupplyChain.Core.FreePointedFunctor as FreePointedFunctor

data FreeMonad f a =
    Step (FreePointedFunctor f a)
  | forall x. Bind (FreeMonad f x) (x -> FreeMonad f a)

pattern Pure :: a -> FreeMonad f a
pattern Pure a = Step (FreePointedFunctor.Pure a)

pattern Map :: f x -> (x -> a) -> FreeMonad f a
pattern Map action extract = Step (FreePointedFunctor.Map action extract)

{-# complete Pure, Map, Bind #-}

deriving instance Functor (FreeMonad f)

instance Applicative (FreeMonad f) where pure = Pure; (<*>) = Monad.ap

instance Monad (FreeMonad f) where (>>=) = Bind

run :: Monad effect =>
    (forall x. f x -> effect x) -- ^ How to interpret @f@ actions
    -> FreeMonad f a -> effect a
run (runEffect :: forall x. f x -> effect x) = recur
  where
    runPF :: FreePointedFunctor f x -> effect x
    runPF = FreePointedFunctor.run runEffect

    recur :: FreeMonad f x -> effect x
    recur = \case
        Step a -> runPF a
        Bind (Step a) b -> runPF a >>= \x -> recur $ b x
        Bind (Bind a b) c -> recur a >>= \x -> recur $ Bind (b x) c

eval :: (forall x. f x -> x) -- ^ How to interpret @f@ actions
    -> FreeMonad f a
    -> a
eval (evalEffect :: forall x. f x -> x) = recur
  where
    evalPF :: FreePointedFunctor f x -> x
    evalPF = FreePointedFunctor.eval evalEffect

    recur :: FreeMonad f x -> x
    recur = \case
        Step a -> evalPF a
        Bind (Step a) b -> evalPF a & \x -> recur $ b x
        Bind (Bind a b) c -> recur a & \x -> recur $ Bind (b x) c

alter :: (forall x. f x -> FreeMonad f' x)
    -> FreeMonad f a -> FreeMonad f' a
alter (f :: forall x. f x -> FreeMonad f' x) = recur
  where
    recur :: FreeMonad f x -> FreeMonad f' x
    recur = \case
        Pure x -> Pure x
        Map action extract -> f action <&> extract
        Bind a b -> Bind (recur a) (recur . b)
