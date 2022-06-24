module Main (main) where

import Hedgehog
import Hedgehog.Main

import qualified Hedgehog.Gen as Gen

import qualified ListT

import qualified Stratoparsec.Document as Doc
import qualified Stratoparsec.Document.Prelude as Doc

main :: IO ()
main = defaultMain [checkParallel (Group "Document parsing examples" docParseExamples)]

abc :: Gen [Text]
abc = Gen.element [["abc"], ["ab", "c"], ["a", "bc"], ["abc", ""], ["", "abc"], ["ab", "", "c"], ["a", "b", "c"]]

abcd :: Gen [Text]
abcd = Gen.element [["abcd"], ["ab", "cd"], ["a", "bcd"], ["abcd", ""], ["abcd", ""], ["ab", "", "cd"], ["a", "b", "c", "d"]]

docParseExamples :: [(PropertyName, Property)]
docParseExamples =
  [ ("Char, char, char", property do
        input <- forAll abc
        x <- Doc.parseOnly Doc.defaultErrorOptions ((,,) <$> Doc.char <*> Doc.char <*> Doc.char <* Doc.end) (ListT.select input)
        x === Right ('a', 'b', 'c')
    )
  , ("Char, char, char, and more", property do
        input <- forAll abcd
        x <- Doc.parseOnly Doc.defaultErrorOptions ((,,) <$> Doc.char <*> Doc.char <*> Doc.char) (ListT.select input)
        x === Right ('a', 'b', 'c')
    )
  , ("Char, char, char, but not more", property do
        input <- forAll abcd
        x <- Doc.parseOnly Doc.defaultErrorOptions ((,,) <$> Doc.char <*> Doc.char <*> Doc.char <* Doc.end) (ListT.select input)
        x === Left (Doc.Error [])
    )
  ]
