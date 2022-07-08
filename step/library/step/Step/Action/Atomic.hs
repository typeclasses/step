{-# language DataKinds, KindSignatures, Trustworthy, TypeFamilies #-}

module Step.Action.Atomic where

import Step.Internal.Prelude

import Step.Action.Types

import Step.Action.Constructors (Any (..), Sure (..))

import qualified Step.Action.Coerce as Coerce

class Atomic (k :: ActionKind)
  where
    type Try k :: ActionKind
    try :: Functor m => k config cursor error m a -> (Try k) config cursor error m (Maybe a)

instance Atomic Atom
  where
    type Try Atom = Sure
    try = Coerce.from @Sure . tryAnySure . Coerce.to @Any

instance Atomic AtomicMove
  where
    type Try AtomicMove = Sure
    try = Coerce.from @Sure . tryAnySure . Coerce.to @Any

instance Atomic Query
  where
    type Try Query = SureQuery
    try = Coerce.from @Sure . tryAnySure . Coerce.to @Any

tryAnySure :: Functor m => Any config cursor error m a -> Sure config cursor error m (Maybe a)
tryAnySure (Any p) = Sure \c -> p c <&> \case
    Left _ -> Nothing
    Right x -> Just x
