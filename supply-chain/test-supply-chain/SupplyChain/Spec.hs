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
  , testCase "parse" $
      let
        x =
            SC.list "12,34ab"
            >-> SC.finiteStreamCursor
            >-> SC.commaSep SC.parseDigits
      in
        SC.eval SC.nil x @?= ["12", "34"]
  ]
