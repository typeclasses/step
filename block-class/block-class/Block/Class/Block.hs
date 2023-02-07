module Block.Class.Block where

import Essentials

import Integer (Positive)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Block.Item (Item)
import Block.Singleton.Class (Singleton)
import Data.Semigroup (sconcat)
import Block.Positional.Class (Positional)
import Block.Search.Class (Search)

import qualified Data.Semigroup as Semigroup

class (Positional xs, Search xs) => Block xs

data Division x c =
    NoDivision | Division (Maybe c) x (Maybe c)
    deriving stock (Eq, Ord, Show, Functor)

data Span c =
    SpanAll
  | SpanNone
  | SpanPart
      { spannedPart :: c
      , spanRemainder :: c
      }
  deriving stock (Eq, Ord, Show, Functor)

data While c =
    WhileNone
  | WhilePrefix c
  | WhileAll
  deriving stock (Eq, Ord, Show, Functor)
