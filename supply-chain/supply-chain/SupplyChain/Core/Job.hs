module SupplyChain.Core.Job
    (Job (FreeMonad, Pure, Effect, Request, Perform, Bind),
    run, eval, alter) where

import Control.Applicative (Applicative)
import Control.Monad (Monad)
import Data.Functor (Functor)
import Data.Function ((.))
import Data.Kind (Type)

import SupplyChain.Core.Kinds (Action, Interface, NoInterface, NoAction)
import SupplyChain.Core.Effect (Effect)
import SupplyChain.Core.FreeMonad (FreeMonad)
import qualified SupplyChain.Core.Effect as Effect
import qualified SupplyChain.Core.FreeMonad as FreeMonad

-- | Monadic context that supports making requests and performing actions
newtype Job (up :: Interface) (action :: Action) (product :: Type) =
    FreeMonad { freeMonad :: FreeMonad (Effect up action) product }

pattern Pure :: product -> Job up action product
pattern Pure product = FreeMonad (FreeMonad.Pure product)

pattern Effect :: Effect up action x -> (x -> product) -> Job up action product
pattern Effect effect extract = FreeMonad (FreeMonad.Map effect extract)

pattern Request :: up x -> (x -> product) -> Job up action product
pattern Request request extract = Effect (Effect.Request request) extract

pattern Perform :: action x -> (x -> product) -> Job up action product
pattern Perform action extract = Effect (Effect.Perform action) extract

pattern Bind :: (Job up action x) -> (x -> Job up action a) -> Job up action a
pattern Bind a b <- FreeMonad (FreeMonad.Bind (FreeMonad -> a) ((FreeMonad .) -> b))
  where Bind a b =  FreeMonad (FreeMonad.Bind (freeMonad    a)  (freeMonad .     b))

{-# complete Pure, Effect, Bind #-}
{-# complete Pure, Request, Perform, Bind #-}

deriving newtype instance Functor     (Job up action)
deriving newtype instance Applicative (Job up action)
deriving newtype instance Monad       (Job up action)

-- | Run a job in its 'Action' context
run :: forall (action :: Action) (product :: Type). Monad action =>
    Job NoInterface action product -> action product
run = FreeMonad.run Effect.run . freeMonad

-- | Run a job that performs no actions
eval :: forall (product :: Type). Job NoInterface NoAction product -> product
eval = FreeMonad.eval Effect.absurd . freeMonad

alter :: forall up up' action action' product.
    (forall x. Effect up action x -> Job up' action' x)
    -> Job up action product -> Job up' action' product
alter f = FreeMonad . FreeMonad.alter (freeMonad . f) . freeMonad
