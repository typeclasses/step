module SupplyChain.Spec (tests) where

import Test.Tasty

import qualified SupplyChain.Interface.TerminableStream.Spec

tests :: TestTree
tests = testGroup "SupplyChain" [SupplyChain.Interface.TerminableStream.Spec.tests]
