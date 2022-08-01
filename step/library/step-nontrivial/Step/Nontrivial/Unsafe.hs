{-# language Unsafe #-}

module Step.Nontrivial.Unsafe
  (
    Nontrivial (NontrivialUnsafe),
    nontrivialUnsafe,
  )
  where

import Step.Internal.Prelude

import Step.Nontrivial.Constructor

import Positive.Unsafe (Positive (PositiveUnsafe))

import qualified ListLike
