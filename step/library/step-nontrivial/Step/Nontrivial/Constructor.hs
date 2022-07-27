{-# language GeneralizedNewtypeDeriving, Unsafe #-}

module Step.Nontrivial.Constructor (Nontrivial (..)) where

import Step.Internal.Prelude

newtype Nontrivial xs x = NontrivialUnsafe xs
    deriving newtype (Semigroup, Eq, Ord, Show)
