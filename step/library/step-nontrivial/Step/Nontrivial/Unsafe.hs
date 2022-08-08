{-# language BangPatterns, GeneralizedNewtypeDeriving, Unsafe #-}

module Step.Nontrivial.Unsafe
  (
    Nontrivial (..)
  )
  where

import Step.Internal.Prelude

import Function (on)

import Show

data Nontrivial xs x =
  NontrivialUnsafe
    { generalize :: !xs
    , length :: Positive Natural
    , head :: x
    , tail :: Maybe (Nontrivial xs x)
    }

instance Eq xs => Eq (Nontrivial xs x) where
    (==) = (==) `on` generalize

instance Ord xs => Ord (Nontrivial xs x) where
    compare = compare `on` generalize

instance Show xs => Show (Nontrivial xs x) where
    showsPrec p = showsPrec p . generalize
