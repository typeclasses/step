{-# language DataKinds, FlexibleContexts, FunctionalDependencies, KindSignatures, Trustworthy, TypeFamilies #-}

module Step.ActionTypes.Atomic where

import Step.Internal.Prelude

import Step.ActionTypes.Types

import Step.ActionTypes.Constructors (Any (..), Sure (..))

import qualified Step.ActionTypes.Coerce as Coerce

import Step.ActionTypes.Functorial

class (FunctorialAction k, FunctorialAction try) => Atomic (k :: ActionKind) (try :: ActionKind) | k -> try
  where
    try :: Functor m => k cursor error m a -> try cursor error m (Maybe a)

instance Atomic Atom Sure
  where
    try = Coerce.from @Sure . tryAnySure . Coerce.to @Any

instance Atomic AtomicMove Sure
  where
    try = Coerce.from @Sure . tryAnySure . Coerce.to @Any

instance Atomic Query SureQuery
  where
    try = Coerce.from @Sure . tryAnySure . Coerce.to @Any

tryAnySure :: Functor m => Any cursor error m a -> Sure cursor error m (Maybe a)
tryAnySure (Any p) = Sure $ p <&> \case
    Left _ -> Nothing
    Right x -> Just x
