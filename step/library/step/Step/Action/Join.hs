module Step.Action.Join where

import Step.Internal.Prelude

import Step.Action.Kinds (ActionKind)
import qualified Step.Action.Kinds as T

import qualified Step.Action.CoercedJoin as CJ

import Step.Action.KindJoin

class ActionJoin (k1 :: ActionKind) (k2 :: ActionKind)
  where
    actionJoin :: Monad m => k1 config cursor error m (k2 config cursor error m a) -> (k1 :> k2) config cursor error m a

-- todo: all 64 instances

instance ActionJoin T.Move       T.Move       where actionJoin = CJ.anyToAny
instance ActionJoin T.MoveAtom   T.MoveAtom   where actionJoin = CJ.anyToAny
instance ActionJoin T.Any        T.Any        where actionJoin = CJ.anyToAny
instance ActionJoin T.Static     T.Static     where actionJoin = CJ.anyToAny
instance ActionJoin T.Atom       T.Atom       where actionJoin = CJ.anyToAny
instance ActionJoin T.Sure       T.Sure       where actionJoin = CJ.sureToSure
instance ActionJoin T.SureStatic T.SureStatic where actionJoin = CJ.sureToSure
instance ActionJoin T.SureMove   T.SureMove   where actionJoin = CJ.sureToSure

instance ActionJoin T.Sure       T.SureMove   where actionJoin = CJ.sureToSure
instance ActionJoin T.SureMove   T.Sure       where actionJoin = CJ.sureToSure
instance ActionJoin T.MoveAtom   T.Move       where actionJoin = CJ.anyToAny
instance ActionJoin T.Move       T.MoveAtom   where actionJoin = CJ.anyToAny
instance ActionJoin T.MoveAtom   T.Any        where actionJoin = CJ.anyToAny
instance ActionJoin T.Any        T.MoveAtom   where actionJoin = CJ.anyToAny
instance ActionJoin T.Move       T.Any        where actionJoin = CJ.anyToAny
instance ActionJoin T.Any        T.Move       where actionJoin = CJ.anyToAny
instance ActionJoin T.Move       T.Atom       where actionJoin = CJ.anyToAny
instance ActionJoin T.Atom       T.Move       where actionJoin = CJ.anyToAny
instance ActionJoin T.Move       T.Static     where actionJoin = CJ.anyToAny
instance ActionJoin T.Static     T.Move       where actionJoin = CJ.anyToAny

instance ActionJoin T.Any        T.SureStatic where actionJoin = CJ.anyToSure
instance ActionJoin T.SureStatic T.Any        where actionJoin = CJ.sureToAny
instance ActionJoin T.Static     T.SureStatic where actionJoin = CJ.anyToSure
instance ActionJoin T.SureStatic T.Static     where actionJoin = CJ.sureToAny
instance ActionJoin T.Move       T.SureStatic where actionJoin = CJ.anyToSure
instance ActionJoin T.SureStatic T.Move       where actionJoin = CJ.sureToAny
instance ActionJoin T.Atom       T.SureStatic where actionJoin = CJ.anyToSure
instance ActionJoin T.SureStatic T.Atom       where actionJoin = CJ.sureToAny
instance ActionJoin T.MoveAtom   T.SureStatic where actionJoin = CJ.anyToSure
instance ActionJoin T.SureStatic T.MoveAtom   where actionJoin = CJ.sureToAny
instance ActionJoin T.Sure       T.SureStatic where actionJoin = CJ.sureToSure
instance ActionJoin T.SureStatic T.Sure       where actionJoin = CJ.sureToSure
instance ActionJoin T.SureMove   T.SureStatic where actionJoin = CJ.sureToSure
instance ActionJoin T.SureStatic T.SureMove   where actionJoin = CJ.sureToSure
instance ActionJoin T.Atom       T.Any        where actionJoin = CJ.anyToAny
instance ActionJoin T.Any        T.Atom       where actionJoin = CJ.anyToAny
instance ActionJoin T.MoveAtom   T.Sure       where actionJoin = CJ.anyToSure
instance ActionJoin T.Sure       T.MoveAtom   where actionJoin = CJ.sureToAny
