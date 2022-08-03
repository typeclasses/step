{-# language DataKinds, FlexibleContexts, MultiParamTypeClasses, KindSignatures, Trustworthy, TypeOperators #-}

module Step.ActionTypes.Join where

import Step.Internal.Prelude

import Step.ActionTypes.Types

import qualified Step.ActionTypes.CoercedJoin as Coerced

import Step.ActionTypes.KindJoin

import Step.ActionTypes.Functorial

class (FunctorialAction act1, FunctorialAction act2, FunctorialAction (act1 >> act2)) =>
    Join (act1 :: Action) (act2 :: Action)
  where
    join :: Monad m => act1 xs x r s m (act2 xs x r s m a) -> (act1 >> act2) xs x r s m a

instance Join Any        Any        where join = Coerced.join
instance Join Any        Atom       where join = Coerced.join
instance Join Any        AtomicMove where join = Coerced.join
instance Join Any        Fail       where join = Coerced.join
instance Join Any        Move       where join = Coerced.join
instance Join Any        Query      where join = Coerced.join
instance Join Any        SureQuery  where join = Coerced.join
instance Join Any        Sure       where join = Coerced.join

instance Join Atom       Any        where join = Coerced.join
instance Join Atom       Atom       where join = Coerced.join
instance Join Atom       AtomicMove where join = Coerced.join
instance Join Atom       Fail       where join = Coerced.join
instance Join Atom       Move       where join = Coerced.join
instance Join Atom       Query      where join = Coerced.join
instance Join Atom       SureQuery  where join = Coerced.join
instance Join Atom       Sure       where join = Coerced.join

instance Join AtomicMove Any        where join = Coerced.join
instance Join AtomicMove Atom       where join = Coerced.join
instance Join AtomicMove AtomicMove where join = Coerced.join
instance Join AtomicMove Fail       where join = Coerced.join
instance Join AtomicMove Move       where join = Coerced.join
instance Join AtomicMove Query      where join = Coerced.join
instance Join AtomicMove SureQuery  where join = Coerced.join
instance Join AtomicMove Sure       where join = Coerced.join

instance Join Fail       Any        where join = Coerced.join
instance Join Fail       Atom       where join = Coerced.join
instance Join Fail       AtomicMove where join = Coerced.join
instance Join Fail       Fail       where join = Coerced.join
instance Join Fail       Move       where join = Coerced.join
instance Join Fail       Query      where join = Coerced.join
instance Join Fail       Sure       where join = Coerced.join
instance Join Fail       SureQuery  where join = Coerced.join

instance Join Move       Any        where join = Coerced.join
instance Join Move       Atom       where join = Coerced.join
instance Join Move       AtomicMove where join = Coerced.join
instance Join Move       Fail       where join = Coerced.join
instance Join Move       Move       where join = Coerced.join
instance Join Move       Query      where join = Coerced.join
instance Join Move       SureQuery  where join = Coerced.join
instance Join Move       Sure       where join = Coerced.join

instance Join Query      Any        where join = Coerced.join
instance Join Query      Atom       where join = Coerced.join
instance Join Query      AtomicMove where join = Coerced.join
instance Join Query      Fail       where join = Coerced.join
instance Join Query      Move       where join = Coerced.join
instance Join Query      Query      where join = Coerced.join
instance Join Query      SureQuery  where join = Coerced.join
instance Join Query      Sure       where join = Coerced.join

instance Join Sure       Any        where join = Coerced.join
instance Join Sure       Atom       where join = Coerced.join
instance Join Sure       AtomicMove where join = Coerced.join
instance Join Sure       Fail       where join = Coerced.join
instance Join Sure       Move       where join = Coerced.join
instance Join Sure       Query      where join = Coerced.join
instance Join Sure       SureQuery  where join = Coerced.join
instance Join Sure       Sure       where join = Coerced.join

instance Join SureQuery  Any        where join = Coerced.join
instance Join SureQuery  Atom       where join = Coerced.join
instance Join SureQuery  AtomicMove where join = Coerced.join
instance Join SureQuery  Fail       where join = Coerced.join
instance Join SureQuery  Move       where join = Coerced.join
instance Join SureQuery  Query      where join = Coerced.join
instance Join SureQuery  Sure       where join = Coerced.join
instance Join SureQuery  SureQuery  where join = Coerced.join
