module SupplyChain.Spec (tests) where

import Prelude

import Control.Monad (replicateM)

import Test.Tasty
import Test.Tasty.HUnit ((@?=), testCase)

import SupplyChain.Base
import qualified SupplyChain.More as SC

tests :: TestTree
tests = testGroup "SupplyChain tests"
  [ testCase "list" $
      let
        x =
            SC.list "abc"
            >-> replicateM 4 (order SC.NextMaybe)
      in
        SC.eval SC.nil x @?= [Just 'a', Just 'b', Just 'c', Nothing]
  , testCase "finiteConcat with more" $
      let
        x =
            SC.list ["a", "bc", "def", "ghij"]
            >-> SC.finiteConcat
            >-> replicateM 5 (order SC.NextMaybe)
      in
        SC.eval SC.nil x @?= [Just 'a', Just 'b', Just 'c', Just 'd', Just 'e']
  , testCase "finiteConcat exhaustion" $
      let
        x =
            SC.list ["a", "bc"]
            >-> SC.finiteConcat
            >-> replicateM 5 (order SC.NextMaybe)
      in
        SC.eval SC.nil x @?= [Just 'a', Just 'b', Just 'c', Nothing, Nothing]
  ]
