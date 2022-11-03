module SupplyChain.Spec (tests) where

import Test.Tasty

import qualified SupplyChain.Bonus.ActionList.Spec
import qualified SupplyChain.Interface.TerminableStream.Spec

tests :: TestTree
tests = testGroup "SupplyChain"
  [ testGroup "Interface"
      [ SupplyChain.Interface.TerminableStream.Spec.tests
      ]
  , testGroup "Bonus"
      [ SupplyChain.Bonus.ActionList.Spec.tests
      ]
  ]
