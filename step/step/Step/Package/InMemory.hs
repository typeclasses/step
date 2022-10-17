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
import SupplyChain ((>->), Factory)

import qualified Data.Sequence as Seq
import qualified Control.Monad.Trans as MTL
import qualified SupplyChain

parse :: forall p c e a. Chunk c => Is p Any =>
    p c Identity e a -> [c] -> (Either e a, [c])
parse p xs = runIdentity (actionParse p xs)

parseMaybe :: forall p c a. Chunk c => Is p Any =>
    p c Identity () a -> [c] -> (Maybe a, [c])
parseMaybe p xs = parse p xs & \(x, r) -> (either (\_ -> Nothing) Just x, r)

parseQuery :: forall p c e a. Chunk c => Is p Query =>
    p c Identity e a -> [c] -> Either e a
parseQuery p xs = runIdentity (actionParseQuery p xs)

parseQueryMaybe :: forall p c a. Chunk c => Is p Query =>
    p c Identity () a -> [c] -> Maybe a
parseQueryMaybe p xs = parseQuery p xs & either (\_ -> Nothing) Just

parseSure :: forall p c a. Chunk c => Is p Sure =>
    p c Identity Void a -> [c] -> (a, [c])
parseSure p xs = runIdentity (actionParseSure p xs)

actionParse :: forall p c m e a. Chunk c => Monad m => Is p Any =>
    p c m e a -> [c] -> m (Either e a, [c])
actionParse p xs = z (run (castTo @Any p)) xs

actionParseQuery :: forall p c m e a. Chunk c => Monad m => Is p Query =>
    p c m e a -> [c] -> m (Either e a)
actionParseQuery p xs = actionParse (castTo @Any (castTo @Query p)) xs <&> \(r, _) -> r

actionParseSure :: forall p c m e a. Chunk c => Monad m => Is p Sure =>
    p c m e a -> [c] -> m (a, [c])
actionParseSure p xs = z (run (castTo @Sure p)) xs

parseSureQuery :: forall p c e a. Chunk c => Is p SureQuery =>
    p c Identity e a -> [c] -> a
parseSureQuery p xs = runIdentity (actionParseSureQuery p xs)

actionParseSureQuery :: forall p c m e a. Chunk c => Monad m => Is p SureQuery =>
    p c m e a -> [c] -> m a
actionParseSureQuery p xs = z (run (castTo @Sure (castTo @SureQuery p))) xs <&> \(r, _) -> r

z :: (Chunk c, Monad f) => Factory (Step 'RW c) f a -> [c] -> f (a, [c])
z parser xs = runStateT (SupplyChain.runFactory (pureStepper (castOptic simple) >-> liftFactory parser)) (Buffer (Seq.fromList xs))
        <&> \(a, rem) -> (a, bufferList rem)

liftFactory :: forall up m m' a. Monad m =>
    MTL.MonadTrans m' => Factory up m a -> Factory up (m' m) a
liftFactory = SupplyChain.actionMap (\(x :: m z) -> MTL.lift x)

bufferList :: Buffer c -> [c]
bufferList (Buffer ys) = toList ys
