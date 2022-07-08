{-# language DataKinds, FlexibleContexts, FunctionalDependencies, KindSignatures, Trustworthy, TypeFamilies #-}

module Step.Action.Atomic where

import Step.Internal.Prelude

import Step.Action.Types

import Step.Action.Constructors (Any (..), Sure (..))

import qualified Step.Action.Coerce as Coerce

import Step.Action.IsAction

class (IsAction k, IsAction try) => Atomic (k :: ActionKind) (try :: ActionKind) | k -> try
  where
    try :: Functor m => k config cursor error m a -> try config cursor error m (Maybe a)

instance Atomic Atom Sure
  where
    try = Coerce.from @Sure . tryAnySure . Coerce.to @Any

instance Atomic AtomicMove Sure
  where
    try = Coerce.from @Sure . tryAnySure . Coerce.to @Any

instance Atomic Query SureQuery
  where
    try = Coerce.from @Sure . tryAnySure . Coerce.to @Any

tryAnySure :: Functor m => Any config cursor error m a -> Sure config cursor error m (Maybe a)
tryAnySure (Any p) = Sure \c -> p c <&> \case
    Left _ -> Nothing
    Right x -> Just x
