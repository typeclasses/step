module SupplyChain.TerminableStream.Spec (tests) where

import Prelude (Maybe (..))

import Control.Monad (replicateM)

import Test.Tasty
import Test.Tasty.HUnit ((@?=), testCase, Assertion)

import SupplyChain
import SupplyChain.TerminableStream

tests :: TestTree
tests = testGroup "TerminableStream"
    [ testCase "finiteList" listTest
    , testGroup "finiteConcat"
        [ testCase "with more" concatTest1
        , testCase "exhaustion" concatTest2
        ]
    ]

listTest :: Assertion
listTest = eval supplyChain @?= result
  where
    supplyChain = list "abc" >-> replicateM 4 (order NextMaybe)
    result = [Just 'a', Just 'b', Just 'c', Nothing]

concatTest1 :: Assertion
concatTest1 = eval supplyChain @?= result
  where
    supplyChain = list ["a", "bc", "def", "ghij"] >-> concat
                    >-> replicateM 5 (order NextMaybe)
    result = [Just 'a', Just 'b', Just 'c', Just 'd', Just 'e']

concatTest2 :: Assertion
concatTest2 = eval supplyChain @?= result
  where
    supplyChain = list ["a", "bc"] >-> concat
                    >-> replicateM 5 (order NextMaybe)
    result = [Just 'a', Just 'b', Just 'c', Nothing, Nothing]
