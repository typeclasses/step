{-# language FlexibleContexts, FunctionalDependencies, TypeOperators, Unsafe #-}

module Step.Action.CoercedJoin
  (
    CoercedJoin, join,
  )
  where

import Step.Internal.Prelude

import Step.Action.Constructors (Any (..), Sure (..))

import Step.Action.Coerce (Coerce)
import qualified Step.Action.Coerce as Coerce

import qualified Monad

import Step.Action.KindJoin

class CoercedJoin k1 k2 k3 | k1 k2 -> k3
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

instance CoercedJoin Any Sure Any
  where
    join = Coerce.from @Any . anyToSure . fmap (Coerce.to @Sure) . Coerce.to @Any

instance CoercedJoin Sure Any Any
  where
    join = Coerce.from @Any . sureToAny . fmap (Coerce.to @Any) . Coerce.to @Sure

anyToSure :: Monad m => Any config cursor error m (Sure config cursor error m a) -> Any config cursor error m a
anyToSure (Any p) =
    Any \c -> do
        e <- p c
        case e of
            Left e' -> return (Left e')
            Right (Sure p') -> Right <$> p' c

sureToAny :: Monad m => Sure config cursor error m (Any config cursor error m a) -> Any config cursor error m a
sureToAny (Sure p) =
    Any \c -> do
        Any p' <- p c
        p' c
