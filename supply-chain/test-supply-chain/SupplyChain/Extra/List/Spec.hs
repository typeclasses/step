module SupplyChain.Extra.List.Spec (tests) where

import Prelude ((<$>), (<*>))

import Test.Tasty
import Test.Tasty.HUnit ((@?=), testCase, Assertion)

import SupplyChain.Extra.List

tests :: TestTree
tests = testGroup "List"
    [ testCase "<*>" apTest
    ]

apTest :: Assertion
apTest =
    toList ((\x y -> [x, y]) <$> fromList "abc" <*> fromList "123")
    @?= ["a1", "a2", "a3", "b1", "b2", "b3", "c1", "c2", "c3"]
