module Main (main) where

import Hedgehog
import Hedgehog.Main

import qualified Hedgehog.Gen as Gen

import qualified ListT
import qualified Char

import qualified Stratoparsec.Document.Base as Doc
import qualified Stratoparsec.Document.Prelude as Doc

import Stratoparsec.Test.InputChunking (genChunks)

main :: IO ()
main = defaultMain [checkParallel (Group "Document parsing examples" docParseExamples)]

char3 :: Doc.Parser (Char, Char, Char)
char3 = (,,) <$> Doc.char <*> Doc.char <*> Doc.char

digit :: Doc.Parser Char
digit = Doc.satisfy Char.isDigit

docParseExamples :: [(PropertyName, Property)]
docParseExamples =
  [ ("Char, char, char", property do
        input <- forAll (genChunks "abc")
        x <- Doc.parseOnly Doc.defaultErrorOptions (char3 <* Doc.end) (ListT.select input)
        x === Right ('a', 'b', 'c')
    )
  , ("Char, char, char, and more", property do
        input <- forAll (genChunks "abcd")
        x <- Doc.parseOnly Doc.defaultErrorOptions char3 (ListT.select input)
        x === Right ('a', 'b', 'c')
    )
  , ("Char, char, char, but not more", property do
        input <- forAll (genChunks "abcd")
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
        input <- forAll (Gen.element ["abc", "abcd"] >>= genChunks)
        x <- Doc.parseOnly Doc.defaultErrorOptions (Doc.text "abc") (ListT.select input)
        x === Right ()
    )
  , ("Expected text", property do
        input <- forAll (Gen.element ["", "ab", "bc"] >>= genChunks)
        x <- Doc.parseOnly Doc.defaultErrorOptions (Doc.text "abc") (ListT.select input)
        x === Left (Doc.Error [])
    )
  ]
