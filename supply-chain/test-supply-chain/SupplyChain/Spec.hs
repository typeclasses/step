module SupplyChain.Spec (tests) where

import Test.Tasty

import qualified SupplyChain.Extra.List.Spec
import qualified SupplyChain.Interface.TerminableStream.Spec

tests :: TestTree
tests = testGroup "SupplyChain"
  [ testGroup "Interface"
      [ SupplyChain.Interface.TerminableStream.Spec.tests
      ]
  , testGroup "Extra"
      [ SupplyChain.Extra.List.Spec.tests
      ]
  ]
