module Main (main) where

import Hedgehog
import Hedgehog.Main

import qualified Hedgehog.Gen as Gen

import qualified ListT
import qualified Char

import qualified Stratoparsec.Document as Doc
import qualified Stratoparsec.Document.Prelude as Doc

main :: IO ()
main = defaultMain [checkParallel (Group "Document parsing examples" docParseExamples)]

abc :: Gen [Text]
abc = Gen.element [["abc"], ["ab", "c"], ["a", "bc"], ["abc", ""], ["", "abc"], ["ab", "", "c"], ["a", "b", "c"]]

abcd :: Gen [Text]
abcd = Gen.element [["abcd"], ["ab", "cd"], ["a", "bcd"], ["abcd", ""], ["abcd", ""], ["ab", "", "cd"], ["a", "b", "c", "d"]]

char3 :: Doc.Parser (Char, Char, Char)
char3 = (,,) <$> Doc.char <*> Doc.char <*> Doc.char

digit :: Doc.Parser Char
digit = Doc.satisfy Char.isDigit

docParseExamples :: [(PropertyName, Property)]
docParseExamples =
  [ ("Char, char, char", property do
        input <- forAll abc
        x <- Doc.parseOnly Doc.defaultErrorOptions (char3 <* Doc.end) (ListT.select input)
        x === Right ('a', 'b', 'c')
    )
  , ("Char, char, char, and more", property do
        input <- forAll abcd
        x <- Doc.parseOnly Doc.defaultErrorOptions char3 (ListT.select input)
        x === Right ('a', 'b', 'c')
    )
  , ("Char, char, char, but not more", property do
        input <- forAll abcd
        x <- Doc.parseOnly Doc.defaultErrorOptions (char3 <* Doc.end) (ListT.select input)
        x === Left (Doc.Error [])
    )
  , ("Digit", property do
        x <- Doc.parseOnly Doc.defaultErrorOptions (Doc.contextualize "Digit" digit) (ListT.select ["2"])
        x === Right '2'
    )
  , ("Expected digit", property do
        x <- Doc.parseOnly Doc.defaultErrorOptions (Doc.contextualize "Digit" digit) (ListT.select ["a"])
        x === Left (Doc.Error ["Digit"])
    )
  , ("Text", property do
        input <- forAll (Gen.choice [abc, abcd])
        x <- Doc.parseOnly Doc.defaultErrorOptions (Doc.text "abc") (ListT.select input)
        x === Right ()
    )
  , ("Expected text", property do
        input <- forAll (Gen.element [[], [""], ["ab"], ["a", "b"], ["bc"]])
        x <- Doc.parseOnly Doc.defaultErrorOptions (Doc.text "abc") (ListT.select input)
        x === Left (Doc.Error [])
    )
  ]
