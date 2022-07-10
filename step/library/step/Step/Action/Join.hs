{-# language DataKinds, FlexibleContexts, MultiParamTypeClasses, KindSignatures, Trustworthy, TypeFamilies, TypeOperators #-}

module Step.Action.Join where

import Step.Internal.Prelude

import Step.Action.Types

import qualified Step.Action.CoercedJoin as Coerced

import Step.Action.KindJoin

import Step.Action.Functorial

class (FunctorialAction k1, FunctorialAction k2, FunctorialAction (k1 >> k2)) => Join (k1 :: ActionKind) (k2 :: ActionKind)
  where
    join :: Monad m => k1 config cursor error m (k2 config cursor error m a) -> (k1 >> k2) config cursor error m a

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
