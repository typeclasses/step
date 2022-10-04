module SupplyChain.Spec (tests) where

tests :: TestTree
tests = testGroup "SupplyChain tests"
  [ testCase "?" (True @?= True)
  ]
