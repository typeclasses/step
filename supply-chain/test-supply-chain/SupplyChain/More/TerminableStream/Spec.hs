module SupplyChain.More.TerminableStream.Spec (tests) where

import Prelude

import Control.Monad (replicateM)

import Test.Tasty
import Test.Tasty.HUnit ((@?=), testCase, Assertion)

import SupplyChain
import SupplyChain.More.TerminableStream

tests :: TestTree
tests = testGroup "SupplyChain.More.TerminableStream"
    [ testCase "finiteList" listTest
    , testGroup "finiteConcat"
        [ testCase "with more" terminableConcat1
        , testCase "exhaustion" terminableConcat2
        ]
    ]

listTest :: Assertion
listTest = eval supplyChain @?= result
  where
    supplyChain = list "abc" >-> replicateM 4 (order NextMaybe)
    result = [Just 'a', Just 'b', Just 'c', Nothing]

terminableConcat1 :: Assertion
terminableConcat1 = eval supplyChain @?= result
  where
    supplyChain = list ["a", "bc", "def", "ghij"] >-> terminableConcat
                    >-> replicateM 5 (order NextMaybe)
    result = [Just 'a', Just 'b', Just 'c', Just 'd', Just 'e']

terminableConcat2 :: Assertion
terminableConcat2 = eval supplyChain @?= result
  where
    supplyChain = list ["a", "bc"] >-> terminableConcat
                    >-> replicateM 5 (order NextMaybe)
    result = [Just 'a', Just 'b', Just 'c', Nothing, Nothing]
