{-# language UndecidableInstances #-}
{-# language DataKinds, KindSignatures, InstanceSigs, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# language ConstraintKinds, DataKinds, FlexibleContexts, KindSignatures, QuantifiedConstraints, TypeOperators #-}
{-# language DataKinds, StandaloneKindSignatures, FunctionalDependencies, FlexibleInstances #-}
{-# language DeriveAnyClass, DeriveFunctor, DerivingVia, GeneralizedNewtypeDeriving, EmptyCase, GADTs #-}

module Step.Effectful where

import Step.Internal.Prelude

import Step.Nontrivial (Nontrivial)
import qualified Step.Nontrivial as Nontrivial

import Positive.Unsafe (Positive (PositiveUnsafe))
import qualified Positive
import qualified Positive.Math as Positive
import qualified Signed

import Effectful
import Effectful.Dispatch.Static
import Effectful.Error.Static

import TypeLits (TypeError, ErrorMessage (Text))

-- ⭕

data Perfection = Perfect | Imperfect

data Flaw (p :: Perfection) (e :: Type) :: Effect

type instance DispatchOf (Flaw p e) = 'Static 'NoSideEffects

-- ⭕

type Commit :: Effect

data Commit :: Effect where
    Commit :: Positive Natural -> Commit m ()

type instance DispatchOf Commit = 'Dynamic

-- ⭕

data Look (xs :: Type) (x :: Type) :: Effect where
    Next :: Look xs x m (Maybe (Nontrivial xs x))
    Reset :: Look xs x m ()

type instance DispatchOf (Look xs x) = 'Dynamic

-- ⭕

-- | The kind of all the action types
type Action =
       Type           -- ^ @xs@ - text
    -> Type           -- ^ @x@ - char
    -> Type           -- ^ @e@ - error
    -> [Effect]       -- ^ @es@ - additional effects
    -> Type           -- ^ @a@ - produced upon success
    -> Type

-- ⭕

type Any :: Action

-- | The most general of the actions
newtype Any xs x e es a = Any (Eff (Commit ': Look xs x ': Error e ': es) a)
    deriving newtype (Functor, Applicative, Monad)

-- ⭕

type Query :: Action

-- | Like 'Any', but cannot move the cursor
newtype Query xs x e es a = Query (Eff (Look xs x ': Error e ': es) a)
    deriving newtype (Functor, Applicative, Monad)

-- ⭕

type Move :: Action

-- | Always moves the cursor
newtype Move xs x e es a = Move (Any xs x e es a)
    deriving stock (Functor)

instance (TypeError ('Text "Move cannot be Applicative because 'pure' would not move the cursor")) =>
    Applicative (Move xs x e es)
  where
    pure = error "unreachable"
    (<*>) = error "unreachable"

-- ⭕

type Atom :: Action

-- | Fails noncommittally; see 'try'
newtype Atom xs x e es a = Atom{ unAtom :: Query xs x e es (Sure xs x e es a) }
    deriving stock (Functor)

instance (TypeError ('Text "Atom cannot be Applicative because (<*>) would not preserve atomicity")) => Applicative (Atom xs x e m) where
    pure = error "unreachable"
    (<*>) = error "unreachable"

-- ⭕

type AtomicMove :: Action

-- | Always moves the cursor, is atomic
newtype AtomicMove xs x e es a = AtomicMove{ unAtomicMove :: Atom xs x e es a }
    deriving stock (Functor)

instance (TypeError ('Text "AtomicMove cannot be Applicative because 'pure' would not move the cursor and (<*>) would not preserve atomicity")) => Applicative (AtomicMove xs x e m) where
    pure = error "unreachable"
    (<*>) = error "unreachable"

-- ⭕

type Sure :: Action

-- | Always succeeds
newtype Sure xs x e es a = Sure (Eff (Commit ': Look xs x ': es) a)
    deriving newtype (Functor, Applicative, Monad)

-- ⭕

type SureQuery :: Action

-- | Always succeeds, does not move the cursor
newtype SureQuery xs x e es a = SureQuery (Eff (Look xs x ': es) a)
    deriving newtype (Functor, Applicative, Monad)
