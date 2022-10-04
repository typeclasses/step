module SupplyChain.Spec (tests) where

import Prelude

import Control.Monad (replicateM)
import Data.Functor.Identity

import Test.Tasty
import Test.Tasty.HUnit ((@?=), testCase)

import SupplyChain.Base
import qualified SupplyChain.More as SC

tests :: TestTree
tests = testGroup "SupplyChain tests"
  [ testCase "list" $
      let
        a = SC.list "abc" ::
              Vendor SC.Nil (SC.FiniteStream Char) Identity
        b = replicateM 4 (order SC.NextMaybe) ::
              Client (SC.FiniteStream Char) Identity [Maybe Char]
      in
        eval SC.nil (a >-> b) @?= [Just 'a', Just 'b', Just 'c', Nothing]
  ]
