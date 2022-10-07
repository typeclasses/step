module Main (main) where

import Test.Tasty

import qualified Step.Toy.Spec

main = defaultMain tests

tests :: TestTree
tests = testGroup "Step"
  [ Step.Toy.Spec.tests
  ]
