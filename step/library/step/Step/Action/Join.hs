module Step.Action.Join where

import Step.Internal.Prelude

import Step.Action.Types

import Step.Action.CoercedJoin

import Step.Action.KindJoin

class ActionJoin (k1 :: ActionKind) (k2 :: ActionKind)
  where
    actionJoin :: Monad m => k1 config cursor error m (k2 config cursor error m a) -> (k1 :> k2) config cursor error m a

-- todo: all 49 instances

instance ActionJoin Move       Move       where actionJoin = anyToAny
instance ActionJoin AtomicMove AtomicMove where actionJoin = anyToAny
instance ActionJoin Any        Any        where actionJoin = anyToAny
instance ActionJoin Query      Query      where actionJoin = anyToAny
instance ActionJoin Atom       Atom       where actionJoin = anyToAny
instance ActionJoin Sure       Sure       where actionJoin = sureToSure
instance ActionJoin SureQuery  SureQuery  where actionJoin = sureToSure

instance ActionJoin AtomicMove Move       where actionJoin = anyToAny
instance ActionJoin Move       AtomicMove where actionJoin = anyToAny
instance ActionJoin AtomicMove Any        where actionJoin = anyToAny
instance ActionJoin Any        AtomicMove where actionJoin = anyToAny
instance ActionJoin Move       Any        where actionJoin = anyToAny
instance ActionJoin Any        Move       where actionJoin = anyToAny
instance ActionJoin Move       Atom       where actionJoin = anyToAny
instance ActionJoin Atom       Move       where actionJoin = anyToAny
instance ActionJoin Move       Query      where actionJoin = anyToAny
instance ActionJoin Query      Move       where actionJoin = anyToAny

instance ActionJoin Any        SureQuery  where actionJoin = anyToSure
instance ActionJoin SureQuery  Any        where actionJoin = sureToAny
instance ActionJoin Query      SureQuery  where actionJoin = anyToSure
instance ActionJoin SureQuery  Query      where actionJoin = sureToAny
instance ActionJoin Move       SureQuery  where actionJoin = anyToSure
instance ActionJoin SureQuery  Move       where actionJoin = sureToAny
instance ActionJoin Atom       SureQuery  where actionJoin = anyToSure
instance ActionJoin SureQuery  Atom       where actionJoin = sureToAny
instance ActionJoin AtomicMove SureQuery  where actionJoin = anyToSure
instance ActionJoin SureQuery  AtomicMove where actionJoin = sureToAny
instance ActionJoin Sure       SureQuery  where actionJoin = sureToSure
instance ActionJoin SureQuery  Sure       where actionJoin = sureToSure
instance ActionJoin Atom       Any        where actionJoin = anyToAny
instance ActionJoin Any        Atom       where actionJoin = anyToAny
instance ActionJoin AtomicMove Sure       where actionJoin = anyToSure
instance ActionJoin Sure       AtomicMove where actionJoin = sureToAny
