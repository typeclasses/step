module Main (main) where

import Step.Internal.Prelude

import Test.Tasty

import qualified Step.Spec.DocumentParsing as DocumentParsing
import qualified Step.Spec.LineHistory as LineHistory
import qualified Step.Spec.Nontrivial as Nontrivial

main = defaultMain tests

tests :: TestTree
tests = testGroup "Step tests" [LineHistory.tests, DocumentParsing.tests, Nontrivial.tests]
