module Step.Action.Join where

import Step.Internal.Prelude

import Optics

import Step.Action.Kinds (ActionKind)
import qualified Step.Action.Kinds as T

import Step.Action.Coerce (Coerce)
import qualified Step.Action.Coerce as Coerce

import qualified Step.Action.SeparateTypes as T

import qualified Monad

import qualified Step.Action.CoercedJoin as CJ

import Step.Action.KindJoin

class ActionJoin (k1 :: ActionKind) (k2 :: ActionKind)
  where
    actionJoin :: Monad m => k1 config cursor error m (k2 config cursor error m a) -> (k1 :> k2) config cursor error m a

-- todo: all 64 instances

instance ActionJoin T.Move       T.Move       where actionJoin = CJ.anyToAny
instance ActionJoin T.MoveUndo   T.MoveUndo   where actionJoin = CJ.anyToAny
instance ActionJoin T.Any        T.Any        where actionJoin = CJ.anyToAny
instance ActionJoin T.Static     T.Static     where actionJoin = CJ.anyToAny
instance ActionJoin T.Undo       T.Undo       where actionJoin = CJ.anyToAny
instance ActionJoin T.Sure       T.Sure       where actionJoin = CJ.sureToSure
instance ActionJoin T.SureStatic T.SureStatic where actionJoin = CJ.sureToSure
instance ActionJoin T.SureMove   T.SureMove   where actionJoin = CJ.sureToSure

instance ActionJoin T.Sure       T.SureMove   where actionJoin = CJ.sureToSure
instance ActionJoin T.SureMove   T.Sure       where actionJoin = CJ.sureToSure
instance ActionJoin T.MoveUndo   T.Move       where actionJoin = CJ.anyToAny
instance ActionJoin T.Move       T.MoveUndo   where actionJoin = CJ.anyToAny
instance ActionJoin T.MoveUndo   T.Any        where actionJoin = CJ.anyToAny
instance ActionJoin T.Any        T.MoveUndo   where actionJoin = CJ.anyToAny
instance ActionJoin T.Move       T.Any        where actionJoin = CJ.anyToAny
instance ActionJoin T.Any        T.Move       where actionJoin = CJ.anyToAny
instance ActionJoin T.Move       T.Undo       where actionJoin = CJ.anyToAny
instance ActionJoin T.Undo       T.Move       where actionJoin = CJ.anyToAny
instance ActionJoin T.Move       T.Static     where actionJoin = CJ.anyToAny
instance ActionJoin T.Static     T.Move       where actionJoin = CJ.anyToAny

instance ActionJoin T.Any        T.SureStatic where actionJoin = CJ.anyToSure
instance ActionJoin T.SureStatic T.Any        where actionJoin = CJ.sureToAny
instance ActionJoin T.Static     T.SureStatic where actionJoin = CJ.anyToSure
instance ActionJoin T.SureStatic T.Static     where actionJoin = CJ.sureToAny
instance ActionJoin T.Move       T.SureStatic where actionJoin = CJ.anyToSure
instance ActionJoin T.SureStatic T.Move       where actionJoin = CJ.sureToAny
instance ActionJoin T.Undo       T.SureStatic where actionJoin = CJ.anyToSure
instance ActionJoin T.SureStatic T.Undo       where actionJoin = CJ.sureToAny
instance ActionJoin T.MoveUndo   T.SureStatic where actionJoin = CJ.anyToSure
instance ActionJoin T.SureStatic T.MoveUndo   where actionJoin = CJ.sureToAny
instance ActionJoin T.Sure       T.SureStatic where actionJoin = CJ.sureToSure
instance ActionJoin T.SureStatic T.Sure       where actionJoin = CJ.sureToSure
instance ActionJoin T.SureMove   T.SureStatic where actionJoin = CJ.sureToSure
instance ActionJoin T.SureStatic T.SureMove   where actionJoin = CJ.sureToSure
instance ActionJoin T.Undo       T.Any        where actionJoin = CJ.anyToAny
instance ActionJoin T.Any        T.Undo       where actionJoin = CJ.anyToAny
instance ActionJoin T.MoveUndo   T.Sure       where actionJoin = CJ.anyToSure
instance ActionJoin T.Sure       T.MoveUndo   where actionJoin = CJ.sureToAny
