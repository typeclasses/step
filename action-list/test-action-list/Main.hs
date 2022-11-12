module Main (main) where

import Data.Functor ((<$>))
import Control.Applicative ((<*>))
import System.IO (IO)

import qualified Data.Char as Char

import Test.Tasty
import Test.Tasty.HUnit ((@?=), testCase, Assertion)

import ActionList

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "ActionList"
    [ testCase "<*>" apTest
    , testCase "group" groupTest
    , testCase "takeWhile" takeWhileTest
    ]

apTest :: Assertion
apTest =
    toList ((\x y -> [x, y]) <$> fromList "abc" <*> fromList "123")
    @?= ["a1", "a2", "a3", "b1", "b2", "b3", "c1", "c2", "c3"]

groupTest :: Assertion
groupTest =
    toList (group (fromList "Hrmm..."))
    @?= [(1, 'H'), (1, 'r'), (2, 'm'), (3, '.')]

takeWhileTest :: Assertion
takeWhileTest =
    toList (takeWhile Char.isUpper (fromList "PORKchop"))
    @?= "PORK"
