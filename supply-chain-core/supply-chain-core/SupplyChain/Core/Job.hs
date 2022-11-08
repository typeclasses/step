-- | Description: a /job/ makes requests, performs actions, and returns

module SupplyChain.Core.Job
  (
    {- * Type -} Job (FreeMonad, Pure, Effect, Request, Perform, Bind),
    {- * Constructing -} effect, perform, order,
    {- * Running -} run, eval,
    {- * Alteration -} alter,
  )
  where

import Control.Applicative (Applicative)
import Control.Monad (Monad)
import Data.Function ((.), id)
import Data.Functor (Functor)
import Data.Functor.Const (Const)
import Data.Void (Void)
import SupplyChain.Core.Effect (Effect)
import SupplyChain.Core.FreeMonad (FreeMonad)

import qualified SupplyChain.Core.Effect as Effect
import qualified SupplyChain.Core.FreeMonad as FreeMonad

{-| Monadic context that supports making requests, performing actions,
    and returning a single result -}
newtype Job up action product =
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
  where Bind a b = FreeMonad (FreeMonad.Bind (freeMonad a) (freeMonad . b))

{-# complete Pure, Effect, Bind #-}
{-# complete Pure, Request, Perform, Bind #-}

deriving instance Functor (Job up action)
deriving instance Applicative (Job up action)
deriving instance Monad (Job up action)

effect :: Effect up action product -> Job up action product
effect x = Effect x id

-- | Send a request via the job's upstream 'Interface'
order :: up product -> Job up action product
order x = Request x id

-- | Perform an action in a job's 'Action' context
perform :: action product -> Job up action product
perform x = Perform x id

-- | Run a job in its 'Action' context
run :: Monad action => Job (Const Void) action product -> action product
run = FreeMonad.run Effect.run . freeMonad

-- | Run a job that performs no actions
eval :: Job (Const Void) (Const Void) product -> product
eval = FreeMonad.eval Effect.absurd . freeMonad

alter :: (forall x. Effect up action x -> Job up' action' x)
    -> Job up action product -> Job up' action' product
alter f = FreeMonad . FreeMonad.alter (freeMonad . f) . freeMonad
