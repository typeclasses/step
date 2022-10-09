module Step.Toy (parse, actionParse) where

import Step.Action.Core
import Step.Chunk.Core
import Step.Base

import Control.Monad (Monad)
import Control.Monad.Except (ExceptT(ExceptT) )
import Control.Monad.State.Strict (runStateT, StateT)
import Data.Either (Either)
import Data.Foldable (toList)
import Data.Function (($))
import Data.Functor (fmap, (<&>))
import Data.Functor.Identity
import Data.Sequence (Seq (..))
import Optics (simple, castOptic)
import SupplyChain ((>->), Factory, Vendor)
import SupplyChain.Interface.TerminableStream (TerminableStream)
import Prelude (String, Char)

import qualified Control.Monad.Trans as MTL
import qualified SupplyChain
import qualified SupplyChain.Interface.TerminableStream as Stream

parse :: forall p c e a. Chunk c => Is p Any =>
    p c Identity e a -> [c] -> (Either e a, [c])
parse p xs = runIdentity (actionParse p xs)

actionParse :: forall p c m e a. Chunk c => Monad m => Is p Any =>
    p c m e a -> [c] -> m (Either e a, [c])
actionParse p xs =
  let
    Any (ExceptT parser) = cast p
  in
    runStateT (SupplyChain.run (cursor xs >-> liftFactory parser)) (Buffer Empty)
        <&> fmap bufferList

liftFactory :: forall up m m' a. Monad m =>
    MTL.MonadTrans m' => Factory up m a -> Factory up (m' m) a
liftFactory = SupplyChain.actionMap (\(x :: m z) -> MTL.lift x)

cursor :: Chunk c => Monad m =>
    [c] -> Vendor up (Step 'RW c) (StateT (Buffer c) m)
cursor xs = Stream.list xs >-> bufferedStepper (castOptic simple)

bufferList :: Buffer c -> [c]
bufferList (Buffer ys) = toList ys
