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
        eval SC.nil (SC.list "abc" >-> replicateM 4 (order SC.NextMaybe))
            @?= [Just 'a', Just 'b', Just 'c', Nothing]
  ]
