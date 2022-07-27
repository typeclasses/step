{-# language DeriveGeneric, FlexibleInstances, FunctionalDependencies, Trustworthy #-}

module Step.Internal.Pair
  (
    Pair (..),
    double,
  )
  where

import BasePrelude (Eq, Ord, Show)
import Generic (Generic)
import Optics (Field1, Field2)

data Pair a = Pair a a
    deriving stock (Eq, Ord, Show, Generic)

double :: x -> Pair x
double x = Pair x x

instance Field1 (Pair a) (Pair a) a a
instance Field2 (Pair a) (Pair a) a a
