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

import Essentials
import Step.Action
import Block.Class.Class
import Step.Interface
import Step.Buffer.State
import Step.Buffer.Buffer
import Step.LeftRight

import Control.Monad.State.Strict (runStateT)
import Data.Either (either)
import Optics (simple, castOptic)
import SupplyChain (Job, (>-))

import qualified Data.Sequence as Seq
import qualified Control.Monad.Trans as MTL
import qualified SupplyChain
import qualified SupplyChain.Alter as Alter

parse :: forall p c r a. Block c => Is p Any =>
    p c Identity r a -> [c] -> r -> (Either r a, [c])
parse p xs r = runIdentity (actionParse p xs r)

parseMaybe :: forall p c r a. Block c => Is p Any =>
    p c Identity r a -> [c] -> r -> (Maybe a, [c])
parseMaybe p xs r = parse p xs r & \(x, rem) -> (either (\_ -> Nothing) Just x, rem)

parseQuery :: forall p c r a. Block c => Is p Query =>
    p c Identity r a -> [c] -> r -> Either r a
parseQuery p xs r = runIdentity (actionParseQuery p xs r)

parseQueryMaybe :: forall p c r a. Block c => Is p Query =>
    p c Identity r a -> [c] -> r -> Maybe a
parseQueryMaybe p xs r = parseQuery p xs r & either (\_ -> Nothing) Just

parseSure :: forall p c r a. Block c => Is p Sure =>
    p c Identity r a -> [c] -> r -> (a, [c])
parseSure p xs r = runIdentity (actionParseSure p xs r)

actionParse :: forall p c m r a. Block c => Monad m => Is p Any =>
    p c m r a -> [c] -> r -> m (Either r a, [c])
actionParse p xs r = z (run r (castTo @Any p)) xs

actionParseQuery :: forall p c m r a. Block c => Monad m => Is p Query =>
    p c m r a -> [c] -> r -> m (Either r a)
actionParseQuery p xs r = actionParse (castTo @Any (castTo @Query p)) xs r <&> \(res, _) -> res

actionParseSure :: forall p c m r a. Block c => Monad m => Is p Sure =>
    p c m r a -> [c] -> r -> m (a, [c])
actionParseSure p xs r = z (getRight <$> run r (castTo @Sure p)) xs

parseSureQuery :: forall p c r a. Block c => Is p SureQuery =>
    p c Identity r a -> [c] -> r -> a
parseSureQuery p xs r = runIdentity (actionParseSureQuery p xs r)

actionParseSureQuery :: forall p c m r a. Block c => Monad m => Is p SureQuery =>
    p c m r a -> [c] -> r -> m a
actionParseSureQuery p xs r = z (run r (castTo @Sure (castTo @SureQuery p))) xs <&> \(res, _) -> getRight res

z :: (Block c, Monad f) => Job (CommittableChunkStream c) f a -> [c] -> f (a, [c])
z parser xs = runStateT (SupplyChain.run (pureStepper (castOptic simple) >- Alter.job' (Alter.perform' MTL.lift) parser)) (Buffer (Seq.fromList xs))
        <&> \(a, List rem) -> (a, rem)
