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
  where


    -- SureQuery is easy: It never changes the kind of whatever it's joined with.

    KindJoin SureQuery k = k
    KindJoin k SureQuery = k

    -- Some of the action kinds compose very nicely with themselves.

    KindJoin Any Any = Any
    KindJoin Move Move = Move
    KindJoin Query Query = Query
    KindJoin Sure Sure = Sure

    -- Atomic, however, lose their atomicity when put in sequence with anything fallible.

    KindJoin AtomicMove AtomicMove = Move
    KindJoin Atom Atom = Any

    -- ...

    KindJoin AtomicMove Move = Move
    KindJoin Move AtomicMove = Move

    KindJoin AtomicMove Any = Move
    KindJoin Any AtomicMove = Move

    KindJoin Move Any = Move
    KindJoin Any Move = Move

    KindJoin Move Atom = Move
    KindJoin Atom Move = Move

    KindJoin Move Query = Move
    KindJoin Query Move = Move

    KindJoin Atom Any = Any
    KindJoin Any Atom = Any

    KindJoin AtomicMove Sure = AtomicMove
    KindJoin Sure AtomicMove = Move

    -- This is a weird one! It is the only example where KindJoin is not commutative.
    -- Atomicity is preserved by composition with a Sure action only if the Atom comes first.

    KindJoin Atom Sure = Atom
    KindJoin Sure Atom = Any
