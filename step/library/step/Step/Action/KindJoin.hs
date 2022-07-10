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

    -- Joining with SureQuery has no effect on the type
    SureQuery >> k = k
    k >> SureQuery = k

    -- Properties other than atomicity are closed under composition.
    Query >> Query = Query
    Sure >> Sure = Sure
    -- (Move >> Move = Move) is covered later below.

    -- When an atomic step is followed by an infallible step, atomicity is preserved.
    Atom >> Sure = Atom
    AtomicMove >> Sure = AtomicMove
    Fail >> k = Fail -- k never fails because it is never reached
    -- (>> SureQuery) has already been covered above.

    -- When an atomic step is preceded by a query, atomicity is preserved.
    Query >> Atom = Atom
    Query >> AtomicMove = AtomicMove
    Query >> Fail = Fail
    -- (SureQuery >>) has already been covered above.

    -- Movement of a part implies movement of the whole.
    k >> Move = Move
    Move >> k = Move

    -- When AtomicMove loses atomicity, it degrades to Move.
    AtomicMove >> _ = Move
    _ >> AtomicMove = Move

    -- All other combinations degrade to Any.
    _ >> _ = Any
