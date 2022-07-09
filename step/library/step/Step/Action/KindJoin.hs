{-# language DataKinds, Safe, TypeFamilies, TypeOperators #-}

module Step.Action.KindJoin where

import Step.Action.Types

type a :> b = KindJoin a b

-- | @KindJoin a b@ tells you what kind of action @do{ a;  b }@ is.
--
-- There is an instance for every pair of action kinds. (todo: there isn't yet)
--
-- This function is mostly commutative (@KindJoin a b@ = @KindJoin b a@), with one exception: @KindJoin 'Atom' 'Sure' = 'Atom'@, but @KindJoin 'Sure' 'Atom' = 'Any'@. Atomicity is preserved by composition with a Sure action only if the Atom comes first.
--
type family KindJoin (k1 :: ActionKind) (k2 :: ActionKind) :: ActionKind

-- SureQuery is easy: It never changes the kind of whatever it's joined with.

type instance KindJoin SureQuery k = k
type instance KindJoin k SureQuery = k

-- Some of the action kinds compose very nicely with themselves.

type instance KindJoin Any        Any        = Any
type instance KindJoin Move       Move       = Move
type instance KindJoin Query      Query      = Query
type instance KindJoin Sure       Sure       = Sure

-- Atomic, however, lose their atomicity when put in sequence with anything fallible.

type instance KindJoin AtomicMove AtomicMove = Move
type instance KindJoin Atom       Atom       = Any

-- ...

type instance KindJoin AtomicMove Move       = Move
type instance KindJoin Move       AtomicMove = Move

type instance KindJoin AtomicMove Any        = Move
type instance KindJoin Any        AtomicMove = Move

type instance KindJoin Move       Any        = Move
type instance KindJoin Any        Move       = Move

type instance KindJoin Move       Atom       = Move
type instance KindJoin Atom       Move       = Move

type instance KindJoin Move       Query      = Move
type instance KindJoin Query      Move       = Move

type instance KindJoin Atom       Any        = Any
type instance KindJoin Any        Atom       = Any

type instance KindJoin AtomicMove Sure       = AtomicMove
type instance KindJoin Sure       AtomicMove = Move

-- This is a weird one! It is the only example where KindJoin is not commutative.
-- Atomicity is preserved by composition with a Sure action only if the Atom comes first.
type instance KindJoin Atom       Sure       = Atom
type instance KindJoin Sure       Atom       = Any
