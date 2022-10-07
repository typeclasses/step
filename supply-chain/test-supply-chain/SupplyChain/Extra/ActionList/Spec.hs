module SupplyChain.Extra.ActionList.Spec (tests) where

import Prelude ((<$>), (<*>))

import Test.Tasty
import Test.Tasty.HUnit ((@?=), testCase, Assertion)

import SupplyChain.Extra.ActionList

tests :: TestTree
tests = testGroup "ActionList"
    [ testCase "<*>" apTest
    ]

apTest :: Assertion
apTest =
    toList ((\x y -> [x, y]) <$> fromList "abc" <*> fromList "123")
    @?= ["a1", "a2", "a3", "b1", "b2", "b3", "c1", "c2", "c3"]
