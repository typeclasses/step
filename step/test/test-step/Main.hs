module Main (main) where

import Step.Internal.Prelude

import Test.Hspec

import qualified Step.Spec.DocumentParsing as DocumentParsing
import qualified Step.Spec.LineHistory as LineHistory

main :: IO ()
main = hspec do
    LineHistory.spec
    DocumentParsing.spec
