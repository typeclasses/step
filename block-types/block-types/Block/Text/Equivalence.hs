module Block.Text.Equivalence
  (
    {- * Character equivalence -}
    asciiCaseInsensitive,
  )
  where

import Essentials
import Block.Class
import Block.Text.Type
import Data.Bool
import Data.Char

import qualified Data.List as List
import qualified Data.Text as Text

asciiCaseInsensitive :: ItemEquivalence Text1
asciiCaseInsensitive =
    ItemEquivalence \(a, b) ->
      ( length a == length b ) &&
      ( Text.zip (generalize a) (generalize b)
          & List.all (\(x, y) -> eq x y)
      )
  where
    eq :: Char -> Char -> Bool
    eq a b = a == b || asciiLower a == asciiLower b

    asciiLower x = if isAsciiUpper x then toLower x else x
