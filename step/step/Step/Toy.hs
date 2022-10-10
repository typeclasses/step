module Step.Toy (parse, actionParse, parseSure, actionParseSure) where

import Step.Action
import Step.Chunk
import Step.Interface

import Control.Monad (Monad)
import Control.Monad.Except (ExceptT(ExceptT) )
import Control.Monad.State.Strict (runStateT, StateT)
import Data.Either (Either, either)
import Data.Foldable (toList)
import Data.Function (($), id)
import Data.Functor (fmap, (<&>))
import Data.Functor.Identity (Identity (runIdentity))
import Data.Sequence (Seq (..))
import Data.Void (Void, absurd)
import Optics (simple, castOptic)
import SupplyChain ((>->), Factory, Vendor)
import SupplyChain.Interface.TerminableStream (TerminableStream)
import Prelude (String, Char)

import qualified Data.Sequence as Seq
import qualified Control.Monad.Trans as MTL
import qualified SupplyChain
import qualified SupplyChain.Interface.TerminableStream as Stream

parse :: forall p c e a. Chunk c => Is p Any =>
    p c Identity e a -> [c] -> (Either e a, [c])
parse p xs = runIdentity (actionParse p xs)

parseSure :: forall p c a. Chunk c => Is p Sure =>
    p c Identity Void a -> [c] -> (a, [c])
parseSure p xs = runIdentity (actionParseSure p xs)

actionParse :: forall p c m e a. Chunk c => Monad m => Is p Any =>
    p c m e a -> [c] -> m (Either e a, [c])
actionParse p xs = let Any (ExceptT parser) = cast p in z parser xs

actionParseSure :: forall p c m a. Chunk c => Monad m => Is p Sure =>
    p c m Void a -> [c] -> m (a, [c])
actionParseSure p xs = let Sure parser = cast p in z parser xs

z :: (Chunk c, Monad f) => Factory (Step 'RW c) f a -> [c] -> f (a, [c])
z parser xs = runStateT (SupplyChain.run (pureStepper (castOptic simple) >-> liftFactory parser)) (Buffer (Seq.fromList xs))
        <&> \(a, rem) -> (a, bufferList rem)

liftFactory :: forall up m m' a. Monad m =>
    MTL.MonadTrans m' => Factory up m a -> Factory up (m' m) a
liftFactory = SupplyChain.actionMap (\(x :: m z) -> MTL.lift x)

bufferList :: Buffer c -> [c]
bufferList (Buffer ys) = toList ys
