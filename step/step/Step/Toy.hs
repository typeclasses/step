module Step.Toy (parse) where

import Step.Action.Core
import Step.Chunk.Unchunked
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

parse :: forall m e a. Monad m => Any (Unchunked Char) m e a -> String -> m (Either e a, String)
parse (Any (ExceptT parser)) xs =
    runStateT (SupplyChain.run (cursor xs >-> liftClient parser)) (Buffer Empty) <&> fmap bufferString

liftClient :: forall up m m' a. Monad m => MTL.MonadTrans m' => Client up m a -> Client up (m' m) a
liftClient = SupplyChain.actionMap (\(x :: m z) -> MTL.lift x)

cursor :: Monad m => [a] -> Vendor up (Step 'RW (Unchunked a)) (StateT (Buffer (Unchunked a)) m)
cursor xs = Stream.list (fmap Unchunked xs) >-> bufferedStepper (castOptic simple)

bufferString :: Buffer (Unchunked Char) -> String
bufferString (Buffer ys) = toList ys <&> \(Unchunked y) -> y
