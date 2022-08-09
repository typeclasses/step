{-# language DataKinds, FlexibleContexts, MultiParamTypeClasses, KindSignatures, TypeOperators #-}

module Step.ActionTypes.Join
  (
    type (>>),
    Join (..)
  )
  where

import Step.Internal.Prelude

import Step.ActionTypes.Constructors

import Step.ActionTypes.Subtyping

import qualified Monad

-- | The type @a >> b@ is type of the expression @a >> b@.
--
-- This function is mostly not commutative (@a >> b@ is not the same as @b >> a@) because whether an atomic action's atomicity is preserved depends on the order of the composition in some cases.

type family (act1 :: Action) >> (act2 :: Action) :: Action
  where

    -- When failure is first, the second step is irrelevant.
    Fail >> k = Fail

    -- When failure is second, sureness and atomicity are lost.
    Sure >> Fail = Any
    SureQuery >> Fail = Query
    Atom >> Fail = Any
    AtomicMove >> Fail = Move
    k >> Fail = k

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
    -- (>> SureQuery) has already been covered above.

    -- When an atomic step is preceded by a query, atomicity is preserved.
    Query >> Atom = Atom
    Query >> Sure = Atom
    Query >> AtomicMove = AtomicMove
    -- (SureQuery >>) has already been covered above.

    -- Movement of a part implies movement of the whole.
    k >> Move = Move
    Move >> k = Move

    -- When AtomicMove loses atomicity, it degrades to Move.
    AtomicMove >> _ = Move
    _ >> AtomicMove = Move

    -- All other combinations degrade to Any.
    _ >> _ = Any

class (FunctorialAction act1, FunctorialAction act2, FunctorialAction (act1 >> act2)) =>
    Join (act1 :: Action) (act2 :: Action)
  where
    join :: Monad m =>
        act1 xs x r s e m (act2 xs x r s e m a)
        -> (act1 >> act2) xs x r s e m a

cast2 :: forall act2 act1 f xs x r s e m a.
    (Is act1 act2, Monad m, Functor f) =>
    f (act1 xs x r s e m a) -> f (act2 xs x r s e m a)
cast2 = fmap castTo

instance Join Any Any where
    join = Monad.join
instance Join Any Atom where
    join = join @Any @Any . cast2 @Any
instance Join Any AtomicMove where
    join = assumeMovement . join @Any @Any . cast2 @Any
instance Join Any Fail where
    join = join @Any @Any . cast2 @Any
instance Join Any Move where
    join = assumeMovement . join @Any @Any . cast2 @Any
instance Join Any Query where
    join = join @Any @Any . cast2 @Any
instance Join Any SureQuery where
    join = join @Any @Any . cast2 @Any
instance Join Any Sure where
    join = join @Any @Any . cast2 @Any

instance Join Atom Any where
    join = join @Any @Any . castTo @Any
instance Join Atom Atom where
    join = join @Any @Any . castTo @Any . cast2 @Any
instance Join Atom AtomicMove where
    join = assumeMovement . join @Any @Any . castTo @Any . cast2 @Any
instance Join Atom Fail where
    join = join @Any @Any . castTo @Any . cast2 @Any
instance Join Atom Move where
    join = assumeMovement . join @Any @Any . castTo @Any . cast2 @Any
instance Join Atom Query where
    join = castTo @Any . join @Any @Any . castTo @Any . cast2 @Any
instance Join Atom SureQuery where
    join = Atom . fmap (join @Sure @SureQuery) . (\(Atom q) -> q)
instance Join Atom Sure where
    join = Atom . fmap (join @Sure @Sure) . (\(Atom q) -> q)

instance Join AtomicMove Any where
    join = assumeMovement . join . castTo @Atom
instance Join AtomicMove Atom where
    join = assumeMovement . join . castTo @Atom
instance Join AtomicMove AtomicMove where
    join = join . castTo @Atom
instance Join AtomicMove Fail where
    join = assumeMovement . join . castTo @Atom
instance Join AtomicMove Move where
    join = join . castTo @Atom
instance Join AtomicMove Query where
    join = assumeMovement . join . castTo @Atom
instance Join AtomicMove SureQuery where
    join = assumeMovement . join . castTo @Atom
instance Join AtomicMove Sure where
    join = assumeMovement . join . castTo @Atom

instance Join Fail Any where
    join (Fail f) = Fail f
instance Join Fail Atom where
    join (Fail f) = Fail f
instance Join Fail AtomicMove where
    join (Fail f) = Fail f
instance Join Fail Fail where
    join (Fail f) = Fail f
instance Join Fail Move where
    join (Fail f) = Fail f
instance Join Fail Query where
    join (Fail f) = Fail f
instance Join Fail Sure where
    join (Fail f) = Fail f
instance Join Fail SureQuery where
    join (Fail f) = Fail f

instance Join Move Any where
    join = assumeMovement . join . castTo @Any
instance Join Move Atom where
    join = assumeMovement . join . castTo @Any
instance Join Move AtomicMove where
    join = join . castTo @Any
instance Join Move Fail where
    join = assumeMovement . join . castTo @Any
instance Join Move Move where
    join = join . castTo @Any
instance Join Move Query where
    join = assumeMovement . join . castTo @Any
instance Join Move SureQuery where
    join = assumeMovement . join . castTo @Any
instance Join Move Sure where
    join = assumeMovement . join . castTo @Any

instance Join Query Any where
    join = join @Any @Any . castTo @Any
instance Join Query Atom where
    join = Atom . join . fmap (\(Atom q) -> q)
instance Join Query AtomicMove where
    join = assumeMovement . join @Query @Atom . cast2 @Atom
instance Join Query Fail where
    join = join @Query @Query . cast2 @Query
instance Join Query Move where
    join = assumeMovement . join @Query @Any . cast2 @Any
instance Join Query Query where
    join = Monad.join
instance Join Query SureQuery where
    join = join @Query @Query . cast2 @Query
instance Join Query Sure where
    join = Atom

instance Join Sure Any where
    join = join @Any @Any . castTo @Any
instance Join Sure Atom where
    join = castTo @Any . join @Any @Any . castTo @Any . cast2 @Any
instance Join Sure AtomicMove where
    join = assumeMovement . join @Any @Any . castTo @Any . cast2 @Any
instance Join Sure Fail where
    join = join @Any @Any . castTo @Any . cast2 @Any
instance Join Sure Move where
    join = assumeMovement . join @Any @Any . castTo @Any . cast2 @Any
instance Join Sure Query where
    join = castTo @Any . join @Any @Any . castTo @Any . cast2 @Any
instance Join Sure SureQuery where
    join = join @Sure @Sure . cast2 @Sure
instance Join Sure Sure where
    join = Monad.join

instance Join SureQuery Any where
    join = join @Any @Any . castTo @Any
instance Join SureQuery Atom where
    join = Atom . join @SureQuery @Query . fmap (\(Atom q) -> q)
instance Join SureQuery AtomicMove where
    join = assumeMovement . join . cast2 @Atom
instance Join SureQuery Fail where
    join = join @Query @Fail . castTo @Query
instance Join SureQuery Move where
    join = assumeMovement . join. cast2 @Any
instance Join SureQuery Query where
    join = join @Query @Query . castTo @Query
instance Join SureQuery Sure where
    join = join @Sure @Sure . castTo @Sure
instance Join SureQuery SureQuery where
    join = Monad.join
