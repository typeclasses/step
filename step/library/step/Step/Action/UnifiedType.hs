module Step.Action.UnifiedType where

import Step.Internal.Prelude

import Optics

import Step.Action.Kinds (ActionKind)
import qualified Step.Action.Kinds as T

import Step.Action.Coerce (Coerce)
import qualified Step.Action.Coerce as Coerce

import qualified Step.Action.SeparateTypes as T

-- import qualified Step.Action.Kind as Kind
-- import Step.Action.Kind ((:>), ActionKind)

import qualified Variado.Monad.Class as V

import qualified Monad

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

instance (Functor m, IsAction k) => Functor (Action k config cursor error m) where
    fmap = under actionIso . fmap

instance (Monad m, IsAction k, T.MonadAction k) => Applicative (Action k config cursor error m) where
    pure = view actionIso . T.pureAction
    (<*>) = Monad.ap

instance (Monad m, IsAction k, T.MonadAction k) => Monad (Action k config cursor error m) where
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
    , forall config cursor error m. Functor m => Functor (k config cursor error m)
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
    IsAction k => Iso
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

class ActionLift (k1 :: ActionKind) (k2 :: ActionKind)
  where
    actionLift :: Monad m => Action k1 config cursor error m a -> Action k2 config cursor error m a

-- todo: all instances, less than 64

instance ActionLift T.Any        T.Any        where actionLift = coerce
instance ActionLift T.Static     T.Static     where actionLift = coerce
instance ActionLift T.Move       T.Move       where actionLift = coerce
instance ActionLift T.Undo       T.Undo       where actionLift = coerce
instance ActionLift T.MoveUndo   T.MoveUndo   where actionLift = coerce
instance ActionLift T.Sure       T.Sure       where actionLift = coerce
instance ActionLift T.SureStatic T.SureStatic where actionLift = coerce
instance ActionLift T.SureMove   T.SureMove   where actionLift = coerce

instance ActionLift T.Move       T.Any  where actionLift = view actionIso . coerce . review actionIso
instance ActionLift T.MoveUndo   T.Any  where actionLift = view actionIso . coerce . review actionIso
instance ActionLift T.MoveUndo   T.Move where actionLift = view actionIso . coerce . review actionIso
instance ActionLift T.Sure       T.Any  where actionLift = view actionIso . Coerce.from @T.Any . T.sureToAny . Coerce.to @T.Sure . review actionIso
instance ActionLift T.SureStatic T.Any  where actionLift = view actionIso . Coerce.from @T.Any . T.sureToAny . Coerce.to @T.Sure . review actionIso
instance ActionLift T.SureStatic T.Sure where actionLift = view actionIso . coerce . review actionIso

actionLiftTo :: forall k2 k1 config cursor error m a.
    Monad m =>
    ActionLift k1 k2 =>
    Action k1 config cursor error m a
    -> Action k2 config cursor error m a
actionLiftTo = actionLift @k1 @k2

---

configure :: (IsAction k, T.ConfigurableAction k) =>
    (config1 -> config2)
    -> Action k config2 cursor error m a
    -> Action k config1 cursor error m a
configure = under actionIso . T.configureAction

---

class ActionJoin (k1 :: ActionKind) (k2 :: ActionKind)
  where
    type (k1 :> k2) :: ActionKind
    actionJoin :: Monad m => Action k1 config cursor error m (Action k2 config cursor error m a) -> Action (k1 :> k2) config cursor error m a

-- todo: all 64 instances

instance ActionJoin T.Sure       T.Sure       where type T.Sure       :> T.Sure       = T.Sure       ; actionJoin = joinSureToSure
instance ActionJoin T.SureStatic T.SureStatic where type T.SureStatic :> T.SureStatic = T.SureStatic ; actionJoin = joinSureToSure
instance ActionJoin T.Move       T.Move       where type T.Move       :> T.Move       = T.Move       ; actionJoin = joinAnyToAny
instance ActionJoin T.MoveUndo   T.MoveUndo   where type T.MoveUndo   :> T.MoveUndo   = T.Move       ; actionJoin = joinAnyToAny
instance ActionJoin T.Any        T.Any        where type T.Any        :> T.Any        = T.Any        ; actionJoin = joinAnyToAny

instance ActionJoin T.Sure       T.SureMove   where type T.Sure       :> T.SureMove   = T.Sure       ; actionJoin = joinSureToSure
instance ActionJoin T.SureMove   T.Sure       where type T.SureMove   :> T.Sure       = T.Sure       ; actionJoin = joinSureToSure

instance ActionJoin T.MoveUndo   T.Move       where type T.MoveUndo   :> T.Move       = T.Move       ; actionJoin = joinAnyToAny
instance ActionJoin T.Move       T.MoveUndo   where type T.Move       :> T.MoveUndo   = T.Move       ; actionJoin = joinAnyToAny
instance ActionJoin T.MoveUndo   T.Any        where type T.MoveUndo   :> T.Any        = T.Move       ; actionJoin = joinAnyToAny
instance ActionJoin T.Any        T.MoveUndo   where type T.Any        :> T.MoveUndo   = T.Move       ; actionJoin = joinAnyToAny
instance ActionJoin T.Move       T.Any        where type T.Move       :> T.Any        = T.Move       ; actionJoin = joinAnyToAny
instance ActionJoin T.Any        T.Move       where type T.Any        :> T.Move       = T.Move       ; actionJoin = joinAnyToAny
instance ActionJoin T.Move       T.Undo       where type T.Move       :> T.Undo       = T.Move       ; actionJoin = joinAnyToAny
instance ActionJoin T.Undo       T.Move       where type T.Undo       :> T.Move       = T.Move       ; actionJoin = joinAnyToAny
instance ActionJoin T.Move       T.Static     where type T.Move       :> T.Static     = T.Move       ; actionJoin = joinAnyToAny
instance ActionJoin T.Static     T.Move       where type T.Static     :> T.Move       = T.Move       ; actionJoin = joinAnyToAny

-- SureStatic is easy: It never changes the kind of whatever it's joined with.

instance ActionJoin T.Any        T.SureStatic where type T.Any        :> T.SureStatic = T.Any        ; actionJoin = joinAnyToSure
instance ActionJoin T.SureStatic T.Any        where type T.SureStatic :> T.Any        = T.Any        ; actionJoin = joinSureToAny

instance ActionJoin T.Static     T.SureStatic where type T.Static     :> T.SureStatic = T.Static     ; actionJoin = joinAnyToSure
instance ActionJoin T.SureStatic T.Static     where type T.SureStatic :> T.Static     = T.Static     ; actionJoin = joinSureToAny

instance ActionJoin T.Move       T.SureStatic where type T.Move       :> T.SureStatic = T.Move       ; actionJoin = joinAnyToSure
instance ActionJoin T.SureStatic T.Move       where type T.SureStatic :> T.Move       = T.Move       ; actionJoin = joinSureToAny

instance ActionJoin T.Undo       T.SureStatic where type T.Undo       :> T.SureStatic = T.Undo       ; actionJoin = joinAnyToSure
instance ActionJoin T.SureStatic T.Undo       where type T.SureStatic :> T.Undo       = T.Undo       ; actionJoin = joinSureToAny

instance ActionJoin T.MoveUndo   T.SureStatic where type T.MoveUndo   :> T.SureStatic = T.MoveUndo   ; actionJoin = joinAnyToSure
instance ActionJoin T.SureStatic T.MoveUndo   where type T.SureStatic :> T.MoveUndo   = T.MoveUndo   ; actionJoin = joinSureToAny

instance ActionJoin T.Sure       T.SureStatic where type T.Sure       :> T.SureStatic = T.Sure       ; actionJoin = joinSureToSure
instance ActionJoin T.SureStatic T.Sure       where type T.SureStatic :> T.Sure       = T.Sure       ; actionJoin = joinSureToSure

instance ActionJoin T.SureMove   T.SureStatic where type T.SureMove   :> T.SureStatic = T.SureMove   ; actionJoin = joinSureToSure
instance ActionJoin T.SureStatic T.SureMove   where type T.SureStatic :> T.SureMove   = T.SureMove   ; actionJoin = joinSureToSure

---

instance (ActionJoin k1 k2, Monad m) => V.PolyMonad (Action k1 config cursor error m) (Action k2 config cursor error m)
  where
    type Join (Action k1 config cursor error m) (Action k2 config cursor error m) = Action (k1 :> k2) config cursor error m
    join = actionJoin

joinAnyToAny :: forall k1 k2 config cursor error m a.
    Monad m =>
    IsAction k1 => Coerce T.Any k1 =>
    IsAction k2 => Coerce T.Any k2 =>
    IsAction (k1 :> k2) => Coerce T.Any (k1 :> k2) =>
    Action k1 config cursor error m (Action k2 config cursor error m a)
    -> Action (k1 :> k2) config cursor error m a
joinAnyToAny = view $
    re (actionIso' @k1)
    % to (Coerce.to @T.Any @k1)
    % to (fmap (view (re (actionIso' @k2) % to (Coerce.to @T.Any @k2))))
    % to Monad.join
    % to (Coerce.from @T.Any @(k1 :> k2))
    % actionIso' @(k1 :> k2)

joinSureToSure :: forall k1 k2 config cursor error m a.
    Monad m =>
    IsAction k1 => Coerce T.Sure k1 =>
    IsAction k2 => Coerce T.Sure k2 =>
    IsAction (k1 :> k2) => Coerce T.Sure (k1 :> k2) =>
    Action k1 config cursor error m (Action k2 config cursor error m a)
    -> Action (k1 :> k2) config cursor error m a
joinSureToSure = view $
    re (actionIso' @k1)
    % to (Coerce.to @T.Sure @k1)
    % to (fmap (view (re (actionIso' @k2) % to (Coerce.to @T.Sure @k2))))
    % to Monad.join
    % to (Coerce.from @T.Sure @(k1 :> k2))
    % actionIso' @(k1 :> k2)

joinAnyToSure :: forall k1 k2 config cursor error m a.
    Monad m =>
    IsAction k1 => Coerce T.Any k1 =>
    IsAction k2 => Coerce T.Sure k2 =>
    IsAction (k1 :> k2) => Coerce T.Any (k1 :> k2) =>
    Action k1 config cursor error m (Action k2 config cursor error m a)
    -> Action (k1 :> k2) config cursor error m a
joinAnyToSure = view $
    re (actionIso' @k1)
    % to (Coerce.to @T.Any @k1)
    % to (fmap (view (re (actionIso' @k2) % to (Coerce.to @T.Sure @k2))))
    % to T.joinAnyToSure
    % to (Coerce.from @T.Any @(k1 :> k2))
    % actionIso' @(k1 :> k2)

joinSureToAny :: forall k1 k2 config cursor error m a.
    Monad m =>
    IsAction k1 => Coerce T.Sure k1 =>
    IsAction k2 => Coerce T.Any k2 =>
    IsAction (k1 :> k2) => Coerce T.Any (k1 :> k2) =>
    Action k1 config cursor error m (Action k2 config cursor error m a)
    -> Action (k1 :> k2) config cursor error m a
joinSureToAny = view $
    re (actionIso' @k1)
    % to (Coerce.to @T.Sure @k1)
    % to (fmap (view (re (actionIso' @k2) % to (Coerce.to @T.Any @k2))))
    % to T.joinSureToAny
    % to (Coerce.from @T.Any @(k1 :> k2))
    % actionIso' @(k1 :> k2)
