module Main (main) where

import Step.Internal.Prelude

import Test.Hspec

import qualified Step.Spec.DocumentParsing as DocumentParsing

main :: IO ()
main = hspec do
    DocumentParsing.spec
