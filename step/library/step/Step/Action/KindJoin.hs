{-# language DataKinds, Safe, TypeFamilies, TypeOperators #-}

module Step.Action.KindJoin where

import Step.Action.Types

-- | The type @a >> b@ is type of the expression @a >> b@.
--
-- There is an instance for every pair of action kinds, 49 in total.
--
-- ==== Non-commutativity
--
-- This function is mostly commutative (@(a >> b) = (b >> a)@), with a few exceptions pertaining to atomicity:
--
-- Atomicity is preserved by composition with a Sure action only if the Atom comes first.
--
-- * @'Atom' >> 'Sure' = 'Atom'@, but @'Sure' >> 'Atom' = 'Any'@.
-- * @'AtomicMove' >> 'Sure' = 'AtomicMove'@, but @'Sure' >> 'AtomicMove' = 'Move'@
--
-- Atomicity is preserved by composition with a Query only if the Atom comes second.
--
-- * @'Query' >> 'Atom' = 'Atom'@, but @'Atom' >> 'Query' = 'Any'@.
-- * @'Query' >> 'AtomicMove' = 'AtomicMove'@, but @'AtomicMove' >> 'Query' = 'Move'@.
--
type family (k1 :: ActionKind) >> (k2 :: ActionKind) :: ActionKind
  where


    -- SureQuery is easy: It never changes the kind of whatever it's joined with.
    -- (15 instances)

    SureQuery >> k = k
    k >> SureQuery = k

    -- Some of the action kinds compose very nicely with themselves.
    -- (4 instances)

    Any >> Any = Any
    Move >> Move = Move
    Query >> Query = Query
    Sure >> Sure = Sure

    -- Atomic, however, lose their atomicity when put in sequence with anything fallible.
    -- (2 instances)

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


    -- Atomicity is preserved by composition with a Sure action only if the Atom comes first.
    -- (4 instances)

    Atom >> Sure = Atom
    Sure >> Atom = Any

    AtomicMove >> Sure = AtomicMove
    Sure >> AtomicMove = Move


    -- Atomicity is preserved by composition with a Query only if the Atom comes second.
    -- (4 instances)

    Query >> Atom = Atom
    Atom >> Query = Any

    Query >> AtomicMove = AtomicMove
    AtomicMove >> Query = Move
