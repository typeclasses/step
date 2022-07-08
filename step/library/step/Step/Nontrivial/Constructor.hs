{-# language DerivingStrategies, GeneralizedNewtypeDeriving #-}

module Step.Nontrivial.Constructor where

import Step.Internal.Prelude

newtype Nontrivial a = Nontrivial a
    deriving newtype (Semigroup, Eq, Ord, Show)
