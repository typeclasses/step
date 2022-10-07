{-# language Safe #-}

module Step.Internal.Times where

import Step.Internal.Dependencies

import qualified Semigroup

times :: Monoid a => Natural -> a -> a
times = Semigroup.mtimesDefault
