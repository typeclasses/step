{-# language BangPatterns, GeneralizedNewtypeDeriving, Unsafe #-}

module Step.Nontrivial.Unsafe
  (
    Nontrivial (..)
  )
  where

import Step.Internal.Prelude

data Nontrivial xs x =
  NontrivialUnsafe
    { generalize :: !xs
    , length :: Positive Natural
    , head :: x
    , tail :: Maybe (Nontrivial xs x)
    }
  deriving stock (Eq, Ord, Show)
