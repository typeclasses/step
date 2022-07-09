{-# language DataKinds, Safe, TypeFamilies, TypeOperators #-}

module Step.Action.KindJoin where

import Step.Action.Types

-- | The type @a >> b@ is type of the expression @a >> b@.
--
-- ==== Non-commutativity
--
-- This function is mostly commutative (@(a >> b) = (b >> a)@), with a few exceptions pertaining to atomicity:
--
-- Atomicity is preserved by composition with a Sure action only if the atomic part comes first.
--
-- * @'Atom' >> 'Sure' = 'Atom'@, but @'Sure' >> 'Atom' = 'Any'@.
-- * @'AtomicMove' >> 'Sure' = 'AtomicMove'@, but @'Sure' >> 'AtomicMove' = 'Move'@
--
-- Atomicity is preserved by composition with a Query only if the atomic part comes second.
--
-- * @'Query' >> 'Atom' = 'Atom'@, but @'Atom' >> 'Query' = 'Any'@.
-- * @'Query' >> 'AtomicMove' = 'AtomicMove'@, but @'AtomicMove' >> 'Query' = 'Move'@.
--
type family (k1 :: ActionKind) >> (k2 :: ActionKind) :: ActionKind
  where

    -- Query and Sure are closed.
    Query >> Query = Query
    Sure >> Sure = Sure

    -- SureQuery is subsumed by whatever it is joined with.
    SureQuery >> k = k
    k >> SureQuery = k

    -- Move subsumes anything else it is joined with.
    k >> Move = Move
    Move >> k = Move

    -- Atomicity is preserved by join with a Sure action if the atomic step comes first.
    Atom       >> Sure = Atom
    AtomicMove >> Sure = AtomicMove

    -- Atomicity is preserved by join with a Query if the atom step comes second.
    Query >> Atom       = Atom
    Query >> AtomicMove = AtomicMove

    -- Atom and AtomicMove lose their atomicity when joined with anything else.

    -- When AtomicMove loses atomicity, it degrades to Move.
    AtomicMove >> _ = Move
    _ >> AtomicMove = Move

    -- All other combinations degrade to Any.
    _ >> _ = Any
