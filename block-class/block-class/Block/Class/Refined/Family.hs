module Block.Class.Refined.Family where

import Essentials

import Data.List.NonEmpty (NonEmpty)

type family Nullable (c :: Type) :: Type

type instance Nullable (NonEmpty xs) = [xs]
