{-# language DataKinds, FlexibleContexts, MultiParamTypeClasses, KindSignatures, Trustworthy, TypeOperators #-}

module Step.ActionTypes.Join where

import Step.Internal.Prelude

import Step.ActionTypes.Constructors

import Step.ActionTypes.KindJoin

import Step.ActionTypes.Functorial

import Step.ActionTypes.Subtyping

import qualified Monad

import qualified Step.ActionTypes.Coerce as Coerce

import Coerce (coerce)

class (FunctorialAction act1, FunctorialAction act2, FunctorialAction (act1 >> act2)) =>
    Join (act1 :: Action) (act2 :: Action)
  where
    join :: Monad m => act1 xs x r s m (act2 xs x r s m a) -> (act1 >> act2) xs x r s m a

instance Join Any Any where
    join = Monad.join
instance Join Any Atom where
    join = join @Any @Any . fmap (cast @Any)
instance Join Any AtomicMove where
    join = coerce . join @Any @Any . fmap (cast @Any)
instance Join Any Fail where
    join = join @Any @Any . fmap (cast @Any)
instance Join Any Move where
    join = coerce . join @Any @Any . fmap (cast @Any)
instance Join Any Query where
    join = join @Any @Any . fmap (cast @Any)
instance Join Any SureQuery where
    join = join @Any @Any . fmap (cast @Any)
instance Join Any Sure where
    join = join @Any @Any . fmap (cast @Any)

instance Join Atom Any where
    join = join @Any @Any . cast @Any
instance Join Atom Atom where
    join = join @Any @Any . cast @Any . fmap (cast @Any)
instance Join Atom AtomicMove where
    join = coerce . join @Any @Any . cast @Any . fmap (cast @Any)
instance Join Atom Fail where
    join = join @Any @Any . cast @Any . fmap (cast @Any)
instance Join Atom Move where
    join = coerce . join @Any @Any . cast @Any . fmap (cast @Any)
instance Join Atom Query where
    join = coerce . join @Any @Any . cast @Any . fmap (cast @Any)
instance Join Atom SureQuery where
    join = coerce . join @Any @Any . cast @Any . fmap (cast @Any)
instance Join Atom Sure where
    join = coerce . join @Any @Any . cast @Any . fmap (cast @Any)

instance Join AtomicMove Any where
    join = coerce . join @Any @Any . cast @Any
instance Join AtomicMove Atom where
    join = coerce . join @Any @Any . cast @Any . fmap (cast @Any)
instance Join AtomicMove AtomicMove where
    join = coerce . join @Any @Any . cast @Any . fmap (cast @Any)
instance Join AtomicMove Fail where
    join = coerce . join @Any @Any . cast @Any . fmap (cast @Any)
instance Join AtomicMove Move where
    join = coerce . join @Any @Any . cast @Any . fmap (cast @Any)
instance Join AtomicMove Query where
    join = coerce . join @Any @Any . cast @Any . fmap (cast @Any)
instance Join AtomicMove SureQuery where
    join = coerce . join @Any @Any . cast @Any . fmap (cast @Any)
instance Join AtomicMove Sure where
    join = coerce . join @Any @Any . cast @Any . fmap (cast @Any)

instance Join Fail Any where
    join (Fail p) = Fail p
instance Join Fail Atom where
    join (Fail p) = Fail p
instance Join Fail AtomicMove where
    join (Fail p) = Fail p
instance Join Fail Fail where
    join (Fail p) = Fail p
instance Join Fail Move where
    join (Fail p) = Fail p
instance Join Fail Query where
    join (Fail p) = Fail p
instance Join Fail Sure where
    join (Fail p) = Fail p
instance Join Fail SureQuery where
    join (Fail p) = Fail p

instance Join Move Any where
    join = coerce . join @Any @Any . cast @Any
instance Join Move Atom where
    join = coerce . join @Any @Any . cast @Any . fmap (cast @Any)
instance Join Move AtomicMove where
    join = coerce . join @Any @Any . cast @Any . fmap (cast @Any)
instance Join Move Fail where
    join = coerce . join @Any @Any . cast @Any . fmap (cast @Any)
instance Join Move Move where
    join = coerce . join @Any @Any . cast @Any . fmap (cast @Any)
instance Join Move Query where
    join = coerce . join @Any @Any . cast @Any . fmap (cast @Any)
instance Join Move SureQuery where
    join = coerce . join @Any @Any . cast @Any . fmap (cast @Any)
instance Join Move Sure where
    join = coerce . join @Any @Any . cast @Any . fmap (cast @Any)

instance Join Query Any where
    join = join @Any @Any . cast @Any
instance Join Query Atom where
    join = coerce . join @Any @Any . cast @Any . fmap (cast @Any)
instance Join Query AtomicMove where
    join = coerce . join @Any @Any . cast @Any . fmap (cast @Any)
instance Join Query Fail where
    join (Query p) = Fail \c -> p c >>= \case
        Left e -> return e
        Right (Fail p') -> p' c
instance Join Query Move where
    join = coerce . join @Any @Any . cast @Any . fmap (cast @Any)
instance Join Query Query where
    join = Monad.join
instance Join Query SureQuery where
    join = join @Query @Query . fmap (cast @Query)
instance Join Query Sure where
    join = coerce . join @Any @Any . cast @Any . fmap (cast @Any)

instance Join Sure Any where
    join = join @Any @Any . cast @Any
instance Join Sure Atom where
    join = coerce . join @Any @Any . cast @Any . fmap (cast @Any)
instance Join Sure AtomicMove where
    join = coerce . join @Any @Any . cast @Any . fmap (cast @Any)
instance Join Sure Fail where
    join = join @Any @Any . cast @Any . fmap (cast @Any)
instance Join Sure Move where
    join = coerce . join @Any @Any . cast @Any . fmap (cast @Any)
instance Join Sure Query where
    join = coerce . join @Any @Any . cast @Any . fmap (cast @Any)
instance Join Sure SureQuery where
    join = join @Sure @Sure . fmap (cast @Sure)
instance Join Sure Sure where
    join = Monad.join

instance Join SureQuery Any where
    join = join @Any @Any . cast @Any
instance Join SureQuery Atom where
    join = coerce . join @Any @Any . cast @Any . fmap (cast @Any)
instance Join SureQuery AtomicMove where
    join = coerce . join @Any @Any . cast @Any . fmap (cast @Any)
instance Join SureQuery Fail where
    join = join @Query @Fail . cast @Query
instance Join SureQuery Move where
    join = coerce . join @Any @Any . cast @Any . fmap (cast @Any)
instance Join SureQuery Query where
    join = join @Query @Query . cast @Query
instance Join SureQuery Sure where
    join = join @Sure @Sure . cast @Sure
instance Join SureQuery SureQuery where
    join = Monad.join
