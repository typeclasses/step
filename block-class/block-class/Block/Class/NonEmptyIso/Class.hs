module Block.Class.NonEmptyIso.Class
  (
    {- * Class -} NonEmptyIso (..),
  )
  where

import Essentials

import Block.Class.Enumerate.Class (Enumerate)
import Block.Class.End (End (..))
import Data.List.NonEmpty (NonEmpty)

import qualified Data.List.NonEmpty as NonEmpty

class (Enumerate x xs) => NonEmptyIso x xs | xs -> x where
    fromNonEmpty :: End -> NonEmpty x -> xs

instance NonEmptyIso x (NonEmpty x) where

    fromNonEmpty :: End -> NonEmpty x -> NonEmpty x
    fromNonEmpty = \case Front -> id; Back -> NonEmpty.reverse
