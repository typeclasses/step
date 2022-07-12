{-# language DerivingStrategies, GeneralizedNewtypeDeriving, Unsafe #-}

module Step.Nontrivial.Constructor where

import Step.Internal.Prelude

newtype Nontrivial a = NontrivialUnsafe a
    deriving newtype (Semigroup, Eq, Ord, Show)
