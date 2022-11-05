{- |

Description: For consuming in-memory input streams

-}

module Step.Package.InMemory
  (
    {- * Any -} parse, actionParse, parseMaybe,
    {- * Query -} parseQuery, actionParseQuery, parseQueryMaybe,
    {- * Sure -} parseSure, actionParseSure,
    {- * SureQuery -} actionParseSureQuery, parseSureQuery,
  )
  where

import Step.Action
import Step.Chunk
import Step.Interface
import Step.Interface.Buffer

import Control.Monad (Monad)
import Control.Monad.State.Strict (runStateT)
import Data.Either (Either (..), either)
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity (runIdentity))
import Data.Maybe (Maybe (..))
import Data.Void (Void)
import Optics (simple, castOptic)
import SupplyChain ((>->), Job)

import qualified Data.Sequence as Seq
import qualified Control.Monad.Trans as MTL
import qualified SupplyChain
import qualified SupplyChain.Alter

parse :: forall p c r e a. Chunk c => Is p Any =>
    p c Identity r e a -> [c] -> r -> (Either e a, [c])
parse p xs r = runIdentity (actionParse p xs r)

parseMaybe :: forall p c r a. Chunk c => Is p Any =>
    p c Identity r () a -> [c] -> r -> (Maybe a, [c])
parseMaybe p xs r = parse p xs r & \(x, rem) -> (either (\_ -> Nothing) Just x, rem)

parseQuery :: forall p c r e a. Chunk c => Is p Query =>
    p c Identity r e a -> [c] -> r -> Either e a
parseQuery p xs r = runIdentity (actionParseQuery p xs r)

parseQueryMaybe :: forall p c r a. Chunk c => Is p Query =>
    p c Identity r () a -> [c] -> r -> Maybe a
parseQueryMaybe p xs r = parseQuery p xs r & either (\_ -> Nothing) Just

parseSure :: forall p c r a. Chunk c => Is p Sure =>
    p c Identity r Void a -> [c] -> r -> (a, [c])
parseSure p xs r = runIdentity (actionParseSure p xs r)

actionParse :: forall p c m r e a. Chunk c => Monad m => Is p Any =>
    p c m r e a -> [c] -> r -> m (Either e a, [c])
actionParse p xs r = z (run r (castTo @Any p)) xs

actionParseQuery :: forall p c m r e a. Chunk c => Monad m => Is p Query =>
    p c m r e a -> [c] -> r -> m (Either e a)
actionParseQuery p xs r = actionParse (castTo @Any (castTo @Query p)) xs r <&> \(res, _) -> res

actionParseSure :: forall p c m r e a. Chunk c => Monad m => Is p Sure =>
    p c m r e a -> [c] -> r -> m (a, [c])
actionParseSure p xs r = z (run r (castTo @Sure p)) xs

parseSureQuery :: forall p c r e a. Chunk c => Is p SureQuery =>
    p c Identity r e a -> [c] -> r -> a
parseSureQuery p xs r = runIdentity (actionParseSureQuery p xs r)

actionParseSureQuery :: forall p c m r e a. Chunk c => Monad m => Is p SureQuery =>
    p c m r e a -> [c] -> r -> m a
actionParseSureQuery p xs r = z (run r (castTo @Sure (castTo @SureQuery p))) xs <&> \(res, _) -> res

z :: (Chunk c, Monad f) => Job (CommittableChunkStream c) f a -> [c] -> f (a, [c])
z parser xs = runStateT (SupplyChain.run (pureStepper (castOptic simple) >-> liftJob parser)) (Buffer (Seq.fromList xs))
        <&> \(a, rem) -> (a, bufferList rem)

liftJob :: forall up m m' a. Monad m =>
    MTL.MonadTrans m' => Job up m a -> Job up (m' m) a
liftJob = SupplyChain.Alter.action' (\(x :: m z) -> MTL.lift x)

bufferList :: Buffer c -> [c]
bufferList (Buffer ys) = toList ys
