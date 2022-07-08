{-# language Trustworthy #-}

module Step.Internal.Positive where

import Step.Internal.Dependencies

import qualified Positive

positive :: (Num n, Ord n) => Prism' n (Positive n)
positive = Positive.refine
