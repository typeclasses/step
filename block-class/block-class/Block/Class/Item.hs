module Block.Class.Item where

import Essentials

import Data.List.NonEmpty (NonEmpty (..))

type family Item (c :: Type) :: Type

type instance Item (NonEmpty x) = x
