{-# language FlexibleContexts, FunctionalDependencies, TypeOperators, Unsafe #-}

module Step.ActionTypes.CoercedJoin
  (
    CoercedJoin, join,
  )
  where

import Step.Internal.Prelude

import Step.ActionTypes.Constructors (Any (..), Fail (..), Sure (..))

import Step.ActionTypes.Coerce (Coerce)
import qualified Step.ActionTypes.Coerce as Coerce

import qualified Monad

import Step.ActionTypes.KindJoin

class CoercedJoin act1 act2 act3
  where
    join :: forall z1 z2 m e a. Monad m =>
        Coerce act1 z1 => Coerce act2 z2 => Coerce act3 (z1 >> z2) =>
        z1 m e (z2 m e a) -> (z1 >> z2) m e a

instance CoercedJoin Any Any Any
  where
    join = Coerce.from @Any . Monad.join . fmap (Coerce.to @Any) . Coerce.to @Any

instance CoercedJoin Sure Sure Sure
  where
    join = Coerce.from @Sure . Monad.join . fmap (Coerce.to @Sure) . Coerce.to @Sure

instance CoercedJoin Fail Fail Fail
  where
    join = Coerce.from @Fail . (\(Fail x) -> Fail x) . Coerce.to @Fail

instance CoercedJoin Any Sure Any
  where
    join =
        Coerce.from @Any
        . (\(Any p) -> Any $ p >>= \case
              Left e' -> return (Left e')
              Right (Sure p') -> Right <$> p')
        . fmap (Coerce.to @Sure) . Coerce.to @Any

instance CoercedJoin Sure Any Any
  where
    join =
        Coerce.from @Any
        . (\(Sure p) -> Any $ p >>= \(Any p') -> p')
        . fmap (Coerce.to @Any) . Coerce.to @Sure

instance CoercedJoin Any Fail Any
  where
    join =
        Coerce.from @Any
        . (\(Any p) -> Any $ p <&> (Left . \case
              Left e' -> e'
              Right (Fail p') -> p'))
        . fmap (Coerce.to @Fail) . Coerce.to @Any

instance CoercedJoin Any Fail Fail
  where
    join =
        Coerce.from @Fail
        . (\(Any p) -> Fail $ p >>= \case
              Left e' -> e'
              Right (Fail p') -> p')
        . fmap (Coerce.to @Fail) . Coerce.to @Any

instance CoercedJoin Sure Fail Any
  where
    join =
        Coerce.from @Any
        . (\(Sure p) -> Any $ p <&> \(Fail p') -> Left p')
        . fmap (Coerce.to @Fail) . Coerce.to @Sure

instance CoercedJoin Sure Fail Fail
  where
    join =
        Coerce.from @Fail
        . (\(Sure p) -> Fail $ p >>= \(Fail p') -> p')
        . fmap (Coerce.to @Fail) . Coerce.to @Sure

instance CoercedJoin Fail Any Fail
  where
    join =
        Coerce.from @Fail . (\(Fail p) -> Fail p) . Coerce.to @Fail

instance CoercedJoin Fail Sure Fail
  where
    join =
        Coerce.from @Fail . (\(Fail p) -> Fail p) . Coerce.to @Fail
