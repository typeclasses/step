{-# language DerivingStrategies, GeneralizedNewtypeDeriving, Unsafe #-}

module Step.Nontrivial.Unsafe where

import Step.Internal.Prelude

newtype Nontrivial xs x = NontrivialUnsafe xs
    deriving newtype (Semigroup, Eq, Ord, Show)
