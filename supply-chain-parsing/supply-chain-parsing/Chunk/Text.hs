module Chunk.Text (Text1, asciiCI) where

import Chunk
import Chunk.Text.Core

import Data.Bool
import Data.Char
import Data.Eq
import Data.Function

import qualified Data.List as List
import qualified Data.Text as Text

asciiCI :: ChunkCharacterEquivalence Text1
asciiCI =
    ChunkCharacterEquivalence $ \a b ->
      ( length a == length b ) &&
      ( Text.zip (generalize a) (generalize b)
          & List.all (\(x, y) -> eq x y)
      )
  where
    eq :: Char -> Char -> Bool
    eq a b = a == b || asciiLower a == asciiLower b

    asciiLower x = if isAsciiUpper x then toLower x else x
