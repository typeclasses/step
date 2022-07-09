{-# language DataKinds, Safe, TypeFamilies, TypeOperators #-}

module Step.Action.KindJoin where

import Step.Action.Types

-- | The type @a >> b@ is type of the expression @a >> b@.
--
-- There is an instance for every pair of action kinds. (todo: there isn't yet)
--
-- This function is mostly commutative (@a >> b@ = @b >> a@), with one exception: @'Atom' >> 'Sure' = 'Atom'@, but @'Sure' >> 'Atom' = 'Any'@. Atomicity is preserved by composition with a Sure action only if the Atom comes first.
--
type family (k1 :: ActionKind) >> (k2 :: ActionKind) :: ActionKind
  where


    -- SureQuery is easy: It never changes the kind of whatever it's joined with.

    SureQuery >> k = k
    k >> SureQuery = k

    -- Some of the action kinds compose very nicely with themselves.

    Any >> Any = Any
    Move >> Move = Move
    Query >> Query = Query
    Sure >> Sure = Sure

    -- Atomic, however, lose their atomicity when put in sequence with anything fallible.

    AtomicMove >> AtomicMove = Move
    Atom >> Atom = Any

    -- ...

    AtomicMove >> Move = Move
    Move >> AtomicMove = Move

    AtomicMove >> Any = Move
    Any >> AtomicMove = Move

    Move >> Any = Move
    Any >> Move = Move

    Move >> Atom = Move
    Atom >> Move = Move

    Move >> Query = Move
    Query >> Move = Move

    Atom >> Any = Any
    Any >> Atom = Any

    AtomicMove >> Sure = AtomicMove
    Sure >> AtomicMove = Move

    -- This is a weird one! It is the only example where (>>) is not commutative.
    -- Atomicity is preserved by composition with a Sure action only if the Atom comes first.

    Atom >> Sure = Atom
    Sure >> Atom = Any
