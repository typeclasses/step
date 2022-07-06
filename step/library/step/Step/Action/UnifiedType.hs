module Step.Action.UnifiedType where

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

import qualified Step.Action.CoercedJoin as CJ

import Step.Action.KindJoin

data Action (k :: ActionKind) config cursor error m a
  where
    Any        :: !(T.Any        config cursor error m a) -> Action T.Any        config cursor error m a
    Static     :: !(T.Static     config cursor error m a) -> Action T.Static     config cursor error m a
    Move       :: !(T.Move       config cursor error m a) -> Action T.Move       config cursor error m a
    Undo       :: !(T.Undo       config cursor error m a) -> Action T.Undo       config cursor error m a
    MoveUndo   :: !(T.MoveUndo   config cursor error m a) -> Action T.MoveUndo   config cursor error m a
    Sure       :: !(T.Sure       config cursor error m a) -> Action T.Sure       config cursor error m a
    SureStatic :: !(T.SureStatic config cursor error m a) -> Action T.SureStatic config cursor error m a
    SureMove   :: !(T.SureMove   config cursor error m a) -> Action T.SureMove   config cursor error m a

instance (Functor m, FunctorAction k, ActionIso k) => Functor (Action k config cursor error m) where
    fmap = under actionIso . fmap

instance (Monad m, FunctorAction k, T.MonadAction k, ActionIso k) => Applicative (Action k config cursor error m) where
    pure = view actionIso . T.pureAction
    (<*>) = Monad.ap

instance (Monad m, FunctorAction k, T.MonadAction k, ActionIso k) => Monad (Action k config cursor error m) where
    a >>= b = view actionIso (T.bindAction (review actionIso a) (fmap (review actionIso) b))

---

class IsAction k => AlwaysMoves k
instance AlwaysMoves T.Move
instance AlwaysMoves T.MoveUndo
instance AlwaysMoves T.SureMove

---

class IsAction k => Noncommittal k
  where
    type Try k :: ActionKind
    try :: Functor m => Action k config cursor error m a -> Action (Try k) config cursor error m (Maybe a)

instance Noncommittal T.Undo
  where
    type Try T.Undo = T.Sure
    try = view (actionIso' @T.Sure) . Coerce.from @T.Sure . T.tryAnySure . Coerce.to @T.Any . review (actionIso' @T.Undo)

instance Noncommittal T.MoveUndo
  where
    type Try T.MoveUndo = T.SureMove
    try = view (actionIso' @T.SureMove) . Coerce.from @T.Sure . T.tryAnySure . Coerce.to @T.Any . review (actionIso' @T.MoveUndo)

instance Noncommittal T.Static
  where
    type Try T.Static = T.SureStatic
    try = view (actionIso' @T.SureStatic) . Coerce.from @T.Sure . T.tryAnySure . Coerce.to @T.Any . review (actionIso' @T.Static)

---

class IsAction k => CanFail k
  where
    failure :: Monad m => (config -> StateT cursor m error) -> Action k config cursor error m a

instance CanFail T.Any      where failure = view (actionIso') . view Coerce.coerced . T.failureAny
instance CanFail T.Static   where failure = view (actionIso') . view Coerce.coerced . T.failureAny
instance CanFail T.Move     where failure = view (actionIso') . view Coerce.coerced . T.failureAny
instance CanFail T.Undo     where failure = view (actionIso') . view Coerce.coerced . T.failureAny
instance CanFail T.MoveUndo where failure = view (actionIso') . view Coerce.coerced . T.failureAny

---

class
    ( SureStaticId k
    , FunctorAction k
    , ActionIso k
    )
    => IsAction (k :: ActionKind)

instance IsAction T.Any
instance IsAction T.Static
instance IsAction T.Move
instance IsAction T.Undo
instance IsAction T.MoveUndo
instance IsAction T.Sure
instance IsAction T.SureStatic
instance IsAction T.SureMove

---

class
    ( ActionJoin k T.SureStatic
    , ActionJoin T.SureStatic k
    , k :> T.SureStatic ~ k
    , T.SureStatic :> k ~ k
    )
    => SureStaticId k

instance SureStaticId T.Any
instance SureStaticId T.Static
instance SureStaticId T.Move
instance SureStaticId T.Undo
instance SureStaticId T.MoveUndo
instance SureStaticId T.Sure
instance SureStaticId T.SureStatic
instance SureStaticId T.SureMove

---

class ActionIso (k :: ActionKind)
  where
    actionIso :: Iso
        (k config1 cursor1 error1 m1 a1)
        (k config2 cursor2 error2 m2 a2)
        (Action k config1 cursor1 error1 m1 a1)
        (Action k config2 cursor2 error2 m2 a2)

actionIso' :: forall k config1 cursor1 error1 m1 a1 config2 cursor2 error2 m2 a2.
    ActionIso k => Iso
        (k config1 cursor1 error1 m1 a1)
        (k config2 cursor2 error2 m2 a2)
        (Action k config1 cursor1 error1 m1 a1)
        (Action k config2 cursor2 error2 m2 a2)
actionIso' = actionIso

instance ActionIso T.Any        where actionIso = iso Any        (\(Any x)        -> x)
instance ActionIso T.Static     where actionIso = iso Static     (\(Static x)     -> x)
instance ActionIso T.Move       where actionIso = iso Move       (\(Move x)       -> x)
instance ActionIso T.Undo       where actionIso = iso Undo       (\(Undo x)       -> x)
instance ActionIso T.MoveUndo   where actionIso = iso MoveUndo   (\(MoveUndo x)   -> x)
instance ActionIso T.Sure       where actionIso = iso Sure       (\(Sure x)       -> x)
instance ActionIso T.SureStatic where actionIso = iso SureStatic (\(SureStatic x) -> x)
instance ActionIso T.SureMove   where actionIso = iso SureMove   (\(SureMove x)   -> x)

---

configure :: (IsAction k, T.ConfigurableAction k) =>
    (config1 -> config2)
    -> Action k config2 cursor error m a
    -> Action k config1 cursor error m a
configure = under actionIso . T.configureAction

---

class ActionJoin (k1 :: ActionKind) (k2 :: ActionKind)
  where
    actionJoin :: Monad m => Action k1 config cursor error m (Action k2 config cursor error m a) -> Action (k1 :> k2) config cursor error m a

-- todo: all 64 instances

instance ActionJoin T.Sure       T.Sure       where actionJoin = cj CJ.sureToSure
instance ActionJoin T.SureStatic T.SureStatic where actionJoin = cj CJ.sureToSure
instance ActionJoin T.Move       T.Move       where actionJoin = cj CJ.anyToAny
instance ActionJoin T.MoveUndo   T.MoveUndo   where actionJoin = cj CJ.anyToAny
instance ActionJoin T.Any        T.Any        where actionJoin = cj CJ.anyToAny
instance ActionJoin T.Sure       T.SureMove   where actionJoin = cj CJ.sureToSure
instance ActionJoin T.SureMove   T.Sure       where actionJoin = cj CJ.sureToSure
instance ActionJoin T.MoveUndo   T.Move       where actionJoin = cj CJ.anyToAny
instance ActionJoin T.Move       T.MoveUndo   where actionJoin = cj CJ.anyToAny
instance ActionJoin T.MoveUndo   T.Any        where actionJoin = cj CJ.anyToAny
instance ActionJoin T.Any        T.MoveUndo   where actionJoin = cj CJ.anyToAny
instance ActionJoin T.Move       T.Any        where actionJoin = cj CJ.anyToAny
instance ActionJoin T.Any        T.Move       where actionJoin = cj CJ.anyToAny
instance ActionJoin T.Move       T.Undo       where actionJoin = cj CJ.anyToAny
instance ActionJoin T.Undo       T.Move       where actionJoin = cj CJ.anyToAny
instance ActionJoin T.Move       T.Static     where actionJoin = cj CJ.anyToAny
instance ActionJoin T.Static     T.Move       where actionJoin = cj CJ.anyToAny

instance ActionJoin T.Any        T.SureStatic where actionJoin = cj CJ.anyToSure
instance ActionJoin T.SureStatic T.Any        where actionJoin = cj CJ.sureToAny
instance ActionJoin T.Static     T.SureStatic where actionJoin = cj CJ.anyToSure
instance ActionJoin T.SureStatic T.Static     where actionJoin = cj CJ.sureToAny
instance ActionJoin T.Move       T.SureStatic where actionJoin = cj CJ.anyToSure
instance ActionJoin T.SureStatic T.Move       where actionJoin = cj CJ.sureToAny
instance ActionJoin T.Undo       T.SureStatic where actionJoin = cj CJ.anyToSure
instance ActionJoin T.SureStatic T.Undo       where actionJoin = cj CJ.sureToAny
instance ActionJoin T.MoveUndo   T.SureStatic where actionJoin = cj CJ.anyToSure
instance ActionJoin T.SureStatic T.MoveUndo   where actionJoin = cj CJ.sureToAny
instance ActionJoin T.Sure       T.SureStatic where actionJoin = cj CJ.sureToSure
instance ActionJoin T.SureStatic T.Sure       where actionJoin = cj CJ.sureToSure
instance ActionJoin T.SureMove   T.SureStatic where actionJoin = cj CJ.sureToSure
instance ActionJoin T.SureStatic T.SureMove   where actionJoin = cj CJ.sureToSure

cj ::
    (ActionIso k1, ActionIso k2, ActionIso (KindJoin k1 k2), Functor (k1 config cursor error m)) =>
    (
      k1 config cursor error m (k2 config cursor error m a)
      -> (k1 :> k2) config cursor error m a
    )
    -> Action k1 config cursor error m (Action k2 config cursor error m a)
    -> Action (k1 :> k2) config cursor error m a
cj f = view actionIso' . f . fmap (review actionIso') . review actionIso'

---

instance (ActionJoin k1 k2, Monad m) => V.PolyMonad (Action k1 config cursor error m) (Action k2 config cursor error m)
  where
    type Join (Action k1 config cursor error m) (Action k2 config cursor error m) = Action (k1 :> k2) config cursor error m
    join = actionJoin
