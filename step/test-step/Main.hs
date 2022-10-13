module Main (main) where

import Test.Tasty

import qualified Step.Package.InMemory.Spec

main = defaultMain tests

tests :: TestTree
tests = testGroup "Step"
  [ Step.Package.InMemory.Spec.tests
  ]
