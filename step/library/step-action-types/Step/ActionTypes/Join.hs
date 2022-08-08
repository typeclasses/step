{-# language DataKinds, FlexibleContexts, MultiParamTypeClasses, KindSignatures, TypeOperators #-}

module Step.ActionTypes.Join
  (
    Join (..)
  )
  where

import Step.Internal.Prelude

import Step.ActionTypes.Constructors

import Step.ActionTypes.KindJoin

import Step.ActionTypes.Functorial

import Step.ActionTypes.Subtyping

import qualified Monad

import Step.ActionTypes.Assume

class (FunctorialAction act1, FunctorialAction act2, FunctorialAction (act1 >> act2)) =>
    Join (act1 :: Action) (act2 :: Action)
  where
    join :: Monad m =>
        act1 xs x r s m (act2 xs x r s m a)
        -> (act1 >> act2) xs x r s m a

cast2 :: forall act2 act1 m f xs x r s a.
    (Is act1 act2, Monad m, Functor f) =>
    f (act1 xs x r s m a) -> f (act2 xs x r s m a)
cast2 = fmap cast

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
    join = join @Any @Any . cast @Any
instance Join Atom Atom where
    join = join @Any @Any . cast @Any . cast2 @Any
instance Join Atom AtomicMove where
    join = assumeMovement . join @Any @Any . cast @Any . cast2 @Any
instance Join Atom Fail where
    join = join @Any @Any . cast @Any . cast2 @Any
instance Join Atom Move where
    join = assumeMovement . join @Any @Any . cast @Any . cast2 @Any
instance Join Atom Query where
    join = cast . join @Any @Any . cast @Any . cast2 @Any
instance Join Atom SureQuery where
    join = Atom . fmap (join @Sure @SureQuery) . (\(Atom q) -> q)
instance Join Atom Sure where
    join = Atom . fmap (join @Sure @Sure) . (\(Atom q) -> q)

instance Join AtomicMove Any where
    join = assumeMovement . join . cast @Atom
instance Join AtomicMove Atom where
    join = assumeMovement . join . cast @Atom
instance Join AtomicMove AtomicMove where
    join = join . cast @Atom
instance Join AtomicMove Fail where
    join = assumeMovement . join . cast @Atom
instance Join AtomicMove Move where
    join = join . cast @Atom
instance Join AtomicMove Query where
    join = assumeMovement . join . cast @Atom
instance Join AtomicMove SureQuery where
    join = assumeMovement . join . cast @Atom
instance Join AtomicMove Sure where
    join = assumeMovement . join . cast @Atom

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
    join = assumeMovement . join . cast @Any
instance Join Move Atom where
    join = assumeMovement . join . cast @Any
instance Join Move AtomicMove where
    join = join . cast @Any
instance Join Move Fail where
    join = assumeMovement . join . cast @Any
instance Join Move Move where
    join = join . cast @Any
instance Join Move Query where
    join = assumeMovement . join . cast @Any
instance Join Move SureQuery where
    join = assumeMovement . join . cast @Any
instance Join Move Sure where
    join = assumeMovement . join . cast @Any

instance Join Query Any where
    join = join @Any @Any . cast @Any
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
    join = join @Any @Any . cast @Any
instance Join Sure Atom where
    join = cast . join @Any @Any . cast @Any . cast2 @Any
instance Join Sure AtomicMove where
    join = assumeMovement . join @Any @Any . cast @Any . cast2 @Any
instance Join Sure Fail where
    join = join @Any @Any . cast @Any . cast2 @Any
instance Join Sure Move where
    join = assumeMovement . join @Any @Any . cast @Any . cast2 @Any
instance Join Sure Query where
    join = cast . join @Any @Any . cast @Any . cast2 @Any
instance Join Sure SureQuery where
    join = join @Sure @Sure . cast2 @Sure
instance Join Sure Sure where
    join = Monad.join

instance Join SureQuery Any where
    join = join @Any @Any . cast @Any
instance Join SureQuery Atom where
    join = Atom . join @SureQuery @Query . fmap (\(Atom q) -> q)
instance Join SureQuery AtomicMove where
    join = assumeMovement . join . cast2 @Atom
instance Join SureQuery Fail where
    join = join @Query @Fail . cast @Query
instance Join SureQuery Move where
    join = assumeMovement . join. cast2 @Any
instance Join SureQuery Query where
    join = join @Query @Query . cast @Query
instance Join SureQuery Sure where
    join = join @Sure @Sure . cast @Sure
instance Join SureQuery SureQuery where
    join = Monad.join
