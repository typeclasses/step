module Step.Toy (parse) where

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
import Data.Sequence (Seq (..))
import Optics (simple, castOptic)
import SupplyChain ((>->), Client, Vendor)
import SupplyChain.Interface.TerminableStream (TerminableStream)
import Prelude (String, Char)

import qualified Control.Monad.Trans as MTL
import qualified SupplyChain
import qualified SupplyChain.Interface.TerminableStream as Stream

parse :: forall c m e a. Chunk c => Monad m =>
    Any c m e a -> [c] -> m (Either e a, [c])
parse (Any (ExceptT parser)) xs =
    runStateT (SupplyChain.run (cursor xs >-> liftClient parser)) (Buffer Empty)
        <&> fmap bufferList

liftClient :: forall up m m' a. Monad m =>
    MTL.MonadTrans m' => Client up m a -> Client up (m' m) a
liftClient = SupplyChain.actionMap (\(x :: m z) -> MTL.lift x)

cursor :: Chunk c => Monad m =>
    [c] -> Vendor up (Step 'RW c) (StateT (Buffer c) m)
cursor xs = Stream.list xs >-> bufferedStepper (castOptic simple)

bufferList :: Buffer c -> [c]
bufferList (Buffer ys) = toList ys
