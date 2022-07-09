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

    -- Types we start with: Any, Query, Move, Atom, AtomicMove, Sure, SureQuery, Failure, AtomicFailure

    -- Query and Sure are closed.
    Query >> Query = Query
    Sure >> Sure = Sure

    -- SureQuery has no effect whatsoever on its surroundings.
    SureQuery >> k = k
    k >> SureQuery = k

    -- Types remaining: Any, Query, Move, Atom, AtomicMove, Sure, Failure, AtomicFailure

    -- Anything followed by atomic failure will fail.
    -- If the atomic failure comes first, the combination is atomic.
    AtomicFailure >> k = AtomicFailure
    k >> AtomicFailure = Failure

    -- Types remaining: Any, Query, Move, Atom, AtomicMove, Sure, Failure

    -- Failure in part is failure in the whole.
    k >> Failure = Failure
    Failure >> k = Failure

    -- Types remaining: Any, Query, Move, Atom, AtomicMove, Sure

    -- Atomicity is preserved by join with a Sure action if the atomic step comes first.
    Atom       >> Sure = Atom
    AtomicMove >> Sure = AtomicMove

    -- Atomicity is preserved by join with a Query if the atom step comes second.
    Query >> Atom       = Atom
    Query >> AtomicMove = AtomicMove

    -- Movement in part is movement in the whole.
    k >> Move = Move
    Move >> k = Move

    -- Types remaining: Any, Query, Atom, AtomicMove, Sure

    -- When AtomicMove loses atomicity, it degrades to Move.
    AtomicMove >> _ = Move
    _ >> AtomicMove = Move

    -- Types remaining: Any, Query, Atom, Sure

    -- All other combinations degrade to Any.
    _ >> _ = Any
