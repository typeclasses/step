{-# language DerivingStrategies, GeneralizedNewtypeDeriving, Unsafe #-}

module Step.Nontrivial.Constructor where

import Step.Internal.Prelude

newtype Nontrivial text char = NontrivialUnsafe text
    deriving newtype (Semigroup, Eq, Ord, Show)
