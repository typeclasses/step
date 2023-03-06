module Block.Class.Construct.Class
  (
    {- * Class -} Construct (..),
  )
  where

import Essentials

import Block.Class.End (End (..))
import Data.List.NonEmpty (NonEmpty)

import qualified Data.List.NonEmpty as NonEmpty

class Construct x xs | xs -> x where
    fromNonEmpty :: End -> NonEmpty x -> xs

instance Construct x (NonEmpty x) where

    fromNonEmpty :: End -> NonEmpty x -> NonEmpty x
    fromNonEmpty = \case Front -> id; Back -> NonEmpty.reverse
