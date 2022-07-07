module Step.Action.Join where

import Step.Internal.Prelude

import Step.Action.Kinds (ActionKind)
import qualified Step.Action.Kinds as T

import qualified Step.Action.CoercedJoin as CJ

import Step.Action.KindJoin

class ActionJoin (k1 :: ActionKind) (k2 :: ActionKind)
  where
    actionJoin :: Monad m => k1 config cursor error m (k2 config cursor error m a) -> (k1 :> k2) config cursor error m a

-- todo: all 49 instances

instance ActionJoin T.Move      T.Move      where actionJoin = CJ.anyToAny
instance ActionJoin T.MoveAtom  T.MoveAtom  where actionJoin = CJ.anyToAny
instance ActionJoin T.Any       T.Any       where actionJoin = CJ.anyToAny
instance ActionJoin T.Query     T.Query     where actionJoin = CJ.anyToAny
instance ActionJoin T.Atom      T.Atom      where actionJoin = CJ.anyToAny
instance ActionJoin T.Sure      T.Sure      where actionJoin = CJ.sureToSure
instance ActionJoin T.SureQuery T.SureQuery where actionJoin = CJ.sureToSure

instance ActionJoin T.MoveAtom  T.Move      where actionJoin = CJ.anyToAny
instance ActionJoin T.Move      T.MoveAtom  where actionJoin = CJ.anyToAny
instance ActionJoin T.MoveAtom  T.Any       where actionJoin = CJ.anyToAny
instance ActionJoin T.Any       T.MoveAtom  where actionJoin = CJ.anyToAny
instance ActionJoin T.Move      T.Any       where actionJoin = CJ.anyToAny
instance ActionJoin T.Any       T.Move      where actionJoin = CJ.anyToAny
instance ActionJoin T.Move      T.Atom      where actionJoin = CJ.anyToAny
instance ActionJoin T.Atom      T.Move      where actionJoin = CJ.anyToAny
instance ActionJoin T.Move      T.Query     where actionJoin = CJ.anyToAny
instance ActionJoin T.Query     T.Move      where actionJoin = CJ.anyToAny

instance ActionJoin T.Any       T.SureQuery where actionJoin = CJ.anyToSure
instance ActionJoin T.SureQuery T.Any       where actionJoin = CJ.sureToAny
instance ActionJoin T.Query     T.SureQuery where actionJoin = CJ.anyToSure
instance ActionJoin T.SureQuery T.Query     where actionJoin = CJ.sureToAny
instance ActionJoin T.Move      T.SureQuery where actionJoin = CJ.anyToSure
instance ActionJoin T.SureQuery T.Move      where actionJoin = CJ.sureToAny
instance ActionJoin T.Atom      T.SureQuery where actionJoin = CJ.anyToSure
instance ActionJoin T.SureQuery T.Atom      where actionJoin = CJ.sureToAny
instance ActionJoin T.MoveAtom  T.SureQuery where actionJoin = CJ.anyToSure
instance ActionJoin T.SureQuery T.MoveAtom  where actionJoin = CJ.sureToAny
instance ActionJoin T.Sure      T.SureQuery where actionJoin = CJ.sureToSure
instance ActionJoin T.SureQuery T.Sure      where actionJoin = CJ.sureToSure
instance ActionJoin T.Atom      T.Any       where actionJoin = CJ.anyToAny
instance ActionJoin T.Any       T.Atom      where actionJoin = CJ.anyToAny
instance ActionJoin T.MoveAtom  T.Sure      where actionJoin = CJ.anyToSure
instance ActionJoin T.Sure      T.MoveAtom  where actionJoin = CJ.sureToAny
