{-# language DataKinds, KindSignatures, MultiParamTypeClasses, Trustworthy, TypeOperators #-}

module Step.Action.Join where

import Step.Internal.Prelude

import Step.Action.Types

import qualified Step.Action.CoercedJoin as Coerced

import Step.Action.KindJoin

import Step.Action.Functorial

class (FunctorialAction k1, FunctorialAction k2) => ActionJoin (k1 :: ActionKind) (k2 :: ActionKind)
  where
    actionJoin :: Monad m => k1 config cursor error m (k2 config cursor error m a) -> (k1 :> k2) config cursor error m a

-- todo: all 49 instances

instance ActionJoin Move       Move       where actionJoin = Coerced.join
instance ActionJoin AtomicMove AtomicMove where actionJoin = Coerced.join
instance ActionJoin Any        Any        where actionJoin = Coerced.join
instance ActionJoin Query      Query      where actionJoin = Coerced.join
instance ActionJoin Atom       Atom       where actionJoin = Coerced.join
instance ActionJoin Sure       Sure       where actionJoin = Coerced.join
instance ActionJoin SureQuery  SureQuery  where actionJoin = Coerced.join

instance ActionJoin AtomicMove Move       where actionJoin = Coerced.join
instance ActionJoin Move       AtomicMove where actionJoin = Coerced.join
instance ActionJoin AtomicMove Any        where actionJoin = Coerced.join
instance ActionJoin Any        AtomicMove where actionJoin = Coerced.join
instance ActionJoin Move       Any        where actionJoin = Coerced.join
instance ActionJoin Any        Move       where actionJoin = Coerced.join
instance ActionJoin Move       Atom       where actionJoin = Coerced.join
instance ActionJoin Atom       Move       where actionJoin = Coerced.join
instance ActionJoin Move       Query      where actionJoin = Coerced.join
instance ActionJoin Query      Move       where actionJoin = Coerced.join

instance ActionJoin Any        SureQuery  where actionJoin = Coerced.join
instance ActionJoin SureQuery  Any        where actionJoin = Coerced.join
instance ActionJoin Query      SureQuery  where actionJoin = Coerced.join
instance ActionJoin SureQuery  Query      where actionJoin = Coerced.join
instance ActionJoin Move       SureQuery  where actionJoin = Coerced.join
instance ActionJoin SureQuery  Move       where actionJoin = Coerced.join
instance ActionJoin Atom       SureQuery  where actionJoin = Coerced.join
instance ActionJoin SureQuery  Atom       where actionJoin = Coerced.join
instance ActionJoin AtomicMove SureQuery  where actionJoin = Coerced.join
instance ActionJoin SureQuery  AtomicMove where actionJoin = Coerced.join
instance ActionJoin Sure       SureQuery  where actionJoin = Coerced.join
instance ActionJoin SureQuery  Sure       where actionJoin = Coerced.join
instance ActionJoin Atom       Any        where actionJoin = Coerced.join
instance ActionJoin Any        Atom       where actionJoin = Coerced.join
instance ActionJoin AtomicMove Sure       where actionJoin = Coerced.join
instance ActionJoin Sure       AtomicMove where actionJoin = Coerced.join
