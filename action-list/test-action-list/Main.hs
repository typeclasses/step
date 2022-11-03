module Main (main) where

import Data.Functor ((<$>))
import Control.Applicative ((<*>))
import System.IO (IO)

import Test.Tasty
import Test.Tasty.HUnit ((@?=), testCase, Assertion)

import ActionList

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "ActionList"
    [ testCase "<*>" apTest
    ]

apTest :: Assertion
apTest =
    toList ((\x y -> [x, y]) <$> fromList "abc" <*> fromList "123")
    @?= ["a1", "a2", "a3", "b1", "b2", "b3", "c1", "c2", "c3"]
