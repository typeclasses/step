module Main (main) where

import Control.Monad (replicateM)
import Data.Char (Char)
import System.IO (IO)
import Integer (Positive)

import Test.Tasty
import Test.Tasty.HUnit ((@?=), testCase, Assertion)

import SupplyChain
import Next.Interface

import qualified Next
import qualified SupplyChain.Job as Job

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
listTest = Job.eval job @?= result
  where
    job = Next.each "abc" >- replicateM 4 (order next)
    result = [Item 'a', Item 'b', Item 'c', End]

concatTest1 :: Assertion
concatTest1 = Job.eval job @?= result
  where
    job = Next.each ["a", "bc", "def", "ghij"] >-> Next.concat >- replicateM 5 (order next)
    result = [Item 'a', Item 'b', Item 'c', Item 'd', Item 'e']

concatTest2 :: Assertion
concatTest2 = Job.eval job @?= result
  where
    job = Next.each ["a", "bc"] >-> Next.concat >- replicateM 5 (order next)
    result = [Item 'a', Item 'b', Item 'c', End, End]

groupLetters :: Assertion
groupLetters = Job.eval job @?= result
  where
    job = Next.each "Hrmm..." >-> Next.group >- Next.toList
    result = [(1, 'H'), (1, 'r'), (2, 'm'), (3, '.')]

groupEmpty :: Assertion
groupEmpty = Job.eval job @?= result
  where
    job = Next.empty >-> Next.group >- Next.toList
    result = [] :: [(Positive, Char)]
