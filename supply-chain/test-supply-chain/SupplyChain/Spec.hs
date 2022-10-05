module SupplyChain.Spec (tests) where

import Prelude

import Control.Monad (replicateM)

import Test.Tasty
import Test.Tasty.HUnit ((@?=), testCase, Assertion)

import SupplyChain.Base
import qualified SupplyChain.More as SC

tests :: TestTree
tests = testGroup "SupplyChain tests"
    [ testCase "list" list
    , testGroup "finiteConcat"
        [ testCase "with more" finiteConcat1
        , testCase "exhaustion" finiteConcat2
        ]
    ]

list :: Assertion
list = SC.eval SC.nil supplyChain @?= result
  where
    supplyChain = SC.list "abc" >-> replicateM 4 (order SC.NextMaybe)
    result = [Just 'a', Just 'b', Just 'c', Nothing]

finiteConcat1 :: Assertion
finiteConcat1 =
    SC.eval SC.nil supplyChain @?= result
  where
    supplyChain = SC.list ["a", "bc", "def", "ghij"] >-> SC.finiteConcat
                    >-> replicateM 5 (order SC.NextMaybe)
    result = [Just 'a', Just 'b', Just 'c', Just 'd', Just 'e']

finiteConcat2 :: Assertion
finiteConcat2 =
    SC.eval SC.nil supplyChain @?= result
  where
    supplyChain = SC.list ["a", "bc"] >-> SC.finiteConcat
                    >-> replicateM 5 (order SC.NextMaybe)
    result = [Just 'a', Just 'b', Just 'c', Nothing, Nothing]
