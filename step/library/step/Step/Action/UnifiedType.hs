module Step.Action.UnifiedType where

import Step.Internal.Prelude

import Step.Action.Kinds (ActionKind)
import qualified Step.Action.Kinds as T

import qualified Step.Action.Coerce as Coerce

import Step.Action.Functor (FunctorAction)

import Step.Action.KindJoin

import Step.Action.Join

import Step.Action.Kinds

class AlwaysMoves (k :: ActionKind)
instance AlwaysMoves T.Move
instance AlwaysMoves T.MoveUndo
instance AlwaysMoves T.SureMove

---

class CanBeStatic (k :: ActionKind)
  where
    trivial :: Monad m => a -> k config cursor error m a

instance CanBeStatic T.Any        where trivial x = T.Any        \_ -> StateT \s -> return (Right x, s)
instance CanBeStatic T.Static     where trivial x = T.Static     \_ -> StateT \s -> return (Right x, s)
instance CanBeStatic T.Undo       where trivial x = T.Undo       \_ -> StateT \s -> return (Right x, s)
instance CanBeStatic T.Sure       where trivial x = T.Sure       \_ -> StateT \s -> return (x, s)
instance CanBeStatic T.SureStatic where trivial x = T.SureStatic \_ -> StateT \s -> return (x, s)

---

type IsAction k =
    ( FunctorAction k
    , ActionJoin k T.SureStatic
    , ActionJoin T.SureStatic k
    , k :> T.SureStatic ~ k
    , T.SureStatic :> k ~ k
    )
