module SupplyChain.Spec (tests) where

import Test.Tasty

import qualified SupplyChain.TerminableStream.Spec

tests :: TestTree
tests = testGroup "SupplyChain" [SupplyChain.TerminableStream.Spec.tests]
