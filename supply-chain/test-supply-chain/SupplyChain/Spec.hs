module SupplyChain.Spec (tests) where

import Test.Tasty

import qualified SupplyChain.More.TerminableStream.Spec

tests :: TestTree
tests = testGroup "SupplyChain" [SupplyChain.More.TerminableStream.Spec.tests]
