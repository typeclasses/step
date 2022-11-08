module Main (main) where

import Data.Maybe (Maybe (..))
import Control.Monad (replicateM)
import Data.Char (Char)
import Data.Function
import Numeric.Natural (Natural)
import System.IO (IO)

import Test.Tasty
import Test.Tasty.HUnit ((@?=), testCase, Assertion)

import SupplyChain
import SupplyChain.Interface.TerminableStream

main :: IO ()
main = defaultMain tests

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
listTest = eval job @?= result
  where
    job = once $ list "abc" >-> loop (replicateM 4 (order NextMaybe))
    result = [Just 'a', Just 'b', Just 'c', Nothing]

concatTest1 :: Assertion
concatTest1 = eval job @?= result
  where
    job = once $ list ["a", "bc", "def", "ghij"] >-> concat
                    >-> loop (replicateM 5 (order NextMaybe))
    result = [Just 'a', Just 'b', Just 'c', Just 'd', Just 'e']

concatTest2 :: Assertion
concatTest2 = eval job @?= result
  where
    job = once $ list ["a", "bc"] >-> concat
                    >-> loop (replicateM 5 (order NextMaybe))
    result = [Just 'a', Just 'b', Just 'c', Nothing, Nothing]

groupLetters :: Assertion
groupLetters = eval job @?= result
  where
    job = once $ list "Hrmm..." >-> group >-> loop all
    result = [(0, 'H'), (0, 'r'), (1, 'm'), (2, '.')]

groupEmpty :: Assertion
groupEmpty = eval job @?= result
  where
    job = once $ nil >-> group >-> loop all
    result = [] :: [(Natural, Char)]
