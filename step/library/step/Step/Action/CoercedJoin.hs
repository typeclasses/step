module Step.Action.CoercedJoin
  (
    anyToAny,
    sureToSure,
    anyToSure,
    sureToAny,
  )
  where

import Step.Internal.Prelude

import Optics

import Step.Action.Kinds

import Step.Action.Coerce (Coerce)
import qualified Step.Action.Coerce as Coerce

import qualified Monad

import Step.Action.KindJoin

anyToAny :: forall k1 k2 config cursor error m a.
    Monad m =>
    Coerce Any k1 =>
    Coerce Any k2 =>
    Coerce Any (k1 :> k2) =>
    k1 config cursor error m (k2 config cursor error m a)
    -> (k1 :> k2) config cursor error m a
anyToAny =
    Coerce.from @Any @(k1 :> k2) . Monad.join . fmap (Coerce.to @Any @k2) . Coerce.to @Any @k1

sureToSure :: forall k1 k2 config cursor error m a.
    Monad m =>
    Coerce Sure k1 =>
    Coerce Sure k2 =>
    Coerce Sure (k1 :> k2) =>
    k1 config cursor error m (k2 config cursor error m a)
    -> (k1 :> k2) config cursor error m a
sureToSure =
    Coerce.from @Sure @(k1 :> k2) . Monad.join . fmap (Coerce.to @Sure @k2) . Coerce.to @Sure @k1

anyToSure :: forall k1 k2 config cursor error m a.
    Monad m =>
    Coerce Any k1 =>
    Coerce Sure k2 =>
    Coerce Any (k1 :> k2) =>
    k1 config cursor error m (k2 config cursor error m a)
    -> (k1 :> k2) config cursor error m a
anyToSure =
    Coerce.from @Any @(k1 :> k2) . anyToSure' . fmap (Coerce.to @Sure @k2) . Coerce.to @Any @k1

sureToAny :: forall k1 k2 config cursor error m a.
    Monad m =>
    Coerce Sure k1 =>
    Coerce Any k2 =>
    Coerce Any (k1 :> k2) =>
    k1 config cursor error m (k2 config cursor error m a)
    -> (k1 :> k2) config cursor error m a
sureToAny =
    Coerce.from @Any @(k1 :> k2) . sureToAny' . fmap (Coerce.to @Any @k2) . Coerce.to @Sure @k1

anyToSure' :: Monad m => Any config cursor error m (Sure config cursor error m a) -> Any config cursor error m a
anyToSure' (Any p) =
    Any \c -> do
        e <- p c
        case e of
            Left e' -> return (Left e')
            Right (Sure p') -> Right <$> p' c

sureToAny' :: Monad m => Sure config cursor error m (Any config cursor error m a) -> Any config cursor error m a
sureToAny' (Sure p) =
    Any \c -> do
        Any p' <- p c
        p' c
