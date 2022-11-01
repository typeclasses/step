module SupplyChain.Core.Spec (tests) where

import Prelude (Maybe (..))

import Control.Monad (replicateM)
import Data.Char (Char)
import Data.Function
import Data.Functor.Identity
import Data.Functor
import Numeric.Natural (Natural)
import Prelude (succ)

import Test.Tasty
import Test.Tasty.HUnit ((@?=), testCase, Assertion)

import SupplyChain.Core

tests :: TestTree
tests = testGroup "Core"
    [ testGroup "Pure"
        [ testCase "run Identity" $ runJob (Pure 'a') () @?= Identity 'a'
        , testCase "run Maybe" $ runJob (Pure 'a') () @?= Just 'a'
        , testCase "eval" $ evalJob (Pure 'a') () @?= 'a'
        ]
    , testGroup "Pure fmapped"
        [ testCase "run Identity" $ runJob (Pure 'a' <&> succ) () @?= Identity 'b'
        , testCase "run Maybe" $ runJob (Pure 'a' <&> succ) () @?= Just 'b'
        , testCase "eval" $ evalJob (Pure 'a' <&> succ) () @?= 'b'
        ]
    ]
