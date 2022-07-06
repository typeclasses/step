module Step.Action.CoercedJoin where

import Step.Internal.Prelude

import Optics

import Step.Action.Kinds (ActionKind)
import qualified Step.Action.Kinds as T

import Step.Action.Coerce (Coerce)
import qualified Step.Action.Coerce as Coerce

import qualified Step.Action.SeparateTypes as T

import Step.Action.Functor (FunctorAction)

import qualified Variado.Monad.Class as V

import qualified Monad

import Step.Action.KindJoin

anyToAny :: forall k1 k2 config cursor error m a.
    Monad m =>
    Coerce T.Any k1 =>
    Coerce T.Any k2 =>
    Coerce T.Any (k1 :> k2) =>
    k1 config cursor error m (k2 config cursor error m a)
    -> (k1 :> k2) config cursor error m a
anyToAny =
    Coerce.from @T.Any @(k1 :> k2) . Monad.join . fmap (Coerce.to @T.Any @k2) . Coerce.to @T.Any @k1

sureToSure :: forall k1 k2 config cursor error m a.
    Monad m =>
    Coerce T.Sure k1 =>
    Coerce T.Sure k2 =>
    Coerce T.Sure (k1 :> k2) =>
    k1 config cursor error m (k2 config cursor error m a)
    -> (k1 :> k2) config cursor error m a
sureToSure =
    Coerce.from @T.Sure @(k1 :> k2) . Monad.join . fmap (Coerce.to @T.Sure @k2) . Coerce.to @T.Sure @k1

anyToSure :: forall k1 k2 config cursor error m a.
    Monad m =>
    Coerce T.Any k1 =>
    Coerce T.Sure k2 =>
    Coerce T.Any (k1 :> k2) =>
    k1 config cursor error m (k2 config cursor error m a)
    -> (k1 :> k2) config cursor error m a
anyToSure =
    Coerce.from @T.Any @(k1 :> k2) . T.joinAnyToSure . fmap (Coerce.to @T.Sure @k2) . Coerce.to @T.Any @k1

sureToAny :: forall k1 k2 config cursor error m a.
    Monad m =>
    Coerce T.Sure k1 =>
    Coerce T.Any k2 =>
    Coerce T.Any (k1 :> k2) =>
    k1 config cursor error m (k2 config cursor error m a)
    -> (k1 :> k2) config cursor error m a
sureToAny =
    Coerce.from @T.Any @(k1 :> k2) . T.joinSureToAny . fmap (Coerce.to @T.Any @k2) . Coerce.to @T.Sure @k1
