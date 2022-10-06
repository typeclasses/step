module SupplyChain.Interface.TerminableStream.Spec (tests) where

import Prelude (Maybe (..))

import Control.Monad (replicateM)
import Data.Char (Char)
import Numeric.Natural (Natural)

import Test.Tasty
import Test.Tasty.HUnit ((@?=), testCase, Assertion)

import SupplyChain
import SupplyChain.Interface.TerminableStream

tests :: TestTree
tests = testGroup "TerminableStream"
    [ testCase "finiteList" listTest
    , testGroup "finiteConcat"
        [ testCase "with more" concatTest1
        , testCase "exhaustion" concatTest2
        ]
    , testGroup "group"
        [ testCase "empty" groupEmpty
        , testCase "letters" groupLetters
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

groupLetters :: Assertion
groupLetters = eval supplyChain @?= result
  where
    supplyChain = (list "Hrmm..." >-> group >-> all)
    result = [(1, 'H'), (1, 'r'), (2, 'm'), (3, '.')]

groupEmpty :: Assertion
groupEmpty = eval supplyChain @?= result
  where
    supplyChain = (nil >-> group >-> all)
    result = [] :: [(Natural, Char)]
