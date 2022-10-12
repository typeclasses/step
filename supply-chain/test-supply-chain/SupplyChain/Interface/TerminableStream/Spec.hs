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
listTest = evalFactory factory @?= result
  where
    factory = list "abc" >-> replicateM 4 (order NextMaybe)
    result = [Just 'a', Just 'b', Just 'c', Nothing]

concatTest1 :: Assertion
concatTest1 = evalFactory factory @?= result
  where
    factory = list ["a", "bc", "def", "ghij"] >-> concat
                    >-> replicateM 5 (order NextMaybe)
    result = [Just 'a', Just 'b', Just 'c', Just 'd', Just 'e']

concatTest2 :: Assertion
concatTest2 = evalFactory factory @?= result
  where
    factory = list ["a", "bc"] >-> concat
                    >-> replicateM 5 (order NextMaybe)
    result = [Just 'a', Just 'b', Just 'c', Nothing, Nothing]

groupLetters :: Assertion
groupLetters = evalFactory factory @?= result
  where
    factory = (list "Hrmm..." >-> group >-> all)
    result = [(0, 'H'), (0, 'r'), (1, 'm'), (2, '.')]

groupEmpty :: Assertion
groupEmpty = evalFactory factory @?= result
  where
    factory = nil >-> group >-> all
    result = [] :: [(Natural, Char)]
