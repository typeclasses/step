{-# language FlexibleContexts, FunctionalDependencies, TypeOperators, Unsafe #-}

module Step.Action.CoercedJoin
  (
    CoercedJoin, join,
  )
  where

import Step.Internal.Prelude

import Step.Action.Constructors (Any (..), Fail (..), Sure (..))

import Step.Action.Coerce (Coerce)
import qualified Step.Action.Coerce as Coerce

import qualified Monad

import Step.Action.KindJoin

class CoercedJoin k1 k2 k3
  where
    join :: forall z1 z2 config cursor error m a. Monad m =>
        Coerce k1 z1 => Coerce k2 z2 => Coerce k3 (z1 >> z2) =>
        z1 config cursor error m (z2 config cursor error m a)
        -> (z1 >> z2) config cursor error m a

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
        Coerce.from @Any . (
            \(Any p) ->
                Any \c -> do
                    e <- p c
                    case e of
                        Left e' -> return (Left e')
                        Right (Sure p') -> Right <$> p' c
        ) . fmap (Coerce.to @Sure) . Coerce.to @Any

instance CoercedJoin Sure Any Any
  where
    join =
        Coerce.from @Any . (
            \(Sure p) ->
                Any \c -> do
                    Any p' <- p c
                    p' c
        ) . fmap (Coerce.to @Any) . Coerce.to @Sure

instance CoercedJoin Any Fail Any
  where
    join =
        Coerce.from @Any . (
            \(Any p) ->
                Any \c ->
                    p c <&> (Left . \case
                        Left e' -> e'
                        Right (Fail p') -> p' c)
        ) . fmap (Coerce.to @Fail) . Coerce.to @Any

instance CoercedJoin Any Fail Fail
  where
    join =
        Coerce.from @Fail . (
            \(Any p) ->
                Fail \c ->
                    p c >>= \case
                        Left e' -> e'
                        Right (Fail p') -> p' c
        ) . fmap (Coerce.to @Fail) . Coerce.to @Any

instance CoercedJoin Sure Fail Any
  where
    join =
        Coerce.from @Any . (
            \(Sure p) ->
                Any \c ->
                    p c <&> \(Fail p') ->
                        Left (p' c)
        ) . fmap (Coerce.to @Fail) . Coerce.to @Sure

instance CoercedJoin Sure Fail Fail
  where
    join =
        Coerce.from @Fail . (
            \(Sure p) ->
                Fail \c ->
                    p c >>= \(Fail p') -> p' c
        ) . fmap (Coerce.to @Fail) . Coerce.to @Sure

instance CoercedJoin Fail Any Fail
  where
    join =
        Coerce.from @Fail . (\(Fail p) -> Fail p) . Coerce.to @Fail

instance CoercedJoin Fail Sure Fail
  where
    join =
        Coerce.from @Fail . (\(Fail p) -> Fail p) . Coerce.to @Fail
