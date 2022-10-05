module SupplyChain.More.TerminableStream.Spec (tests) where

import Prelude

import Control.Monad (replicateM)

import Test.Tasty
import Test.Tasty.HUnit ((@?=), testCase, Assertion)

import SupplyChain
import qualified SupplyChain.More as SC

tests :: TestTree
tests = testGroup "SupplyChain.More.TerminableStream"
    [ testCase "finiteList" list
    , testGroup "finiteConcat"
        [ testCase "with more" terminableConcat1
        , testCase "exhaustion" terminableConcat2
        ]
    ]

list :: Assertion
list = eval supplyChain @?= result
  where
    supplyChain = SC.list "abc" >-> replicateM 4 (order SC.NextMaybe)
    result = [Just 'a', Just 'b', Just 'c', Nothing]

terminableConcat1 :: Assertion
terminableConcat1 =
    eval supplyChain @?= result
  where
    supplyChain = SC.list ["a", "bc", "def", "ghij"] >-> SC.terminableConcat
                    >-> replicateM 5 (order SC.NextMaybe)
    result = [Just 'a', Just 'b', Just 'c', Just 'd', Just 'e']

terminableConcat2 :: Assertion
terminableConcat2 =
    eval supplyChain @?= result
  where
    supplyChain = SC.list ["a", "bc"] >-> SC.terminableConcat
                    >-> replicateM 5 (order SC.NextMaybe)
    result = [Just 'a', Just 'b', Just 'c', Nothing, Nothing]
