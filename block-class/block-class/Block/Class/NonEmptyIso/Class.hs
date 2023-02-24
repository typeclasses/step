module Block.Class.NonEmptyIso.Class
  (
    {- * Class -} NonEmptyIso (..),
  )
  where

import Essentials

import Block.Class.End (End (..))
import Data.List.NonEmpty (NonEmpty)

import qualified Data.List.NonEmpty as NonEmpty

class NonEmptyIso x xs | xs -> x where
    toNonEmpty :: End -> xs -> NonEmpty x
    fromNonEmpty :: End -> NonEmpty x -> xs

instance NonEmptyIso x (NonEmpty x) where

    toNonEmpty :: End -> NonEmpty x -> NonEmpty x
    toNonEmpty = \case Front -> id; Back -> NonEmpty.reverse

    fromNonEmpty :: End -> NonEmpty x -> NonEmpty x
    fromNonEmpty = \case Front -> id; Back -> NonEmpty.reverse
