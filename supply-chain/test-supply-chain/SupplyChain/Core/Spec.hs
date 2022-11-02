module SupplyChain.Core.Spec (tests) where

import Prelude (Maybe (..))

import Data.Function
import Data.Functor.Identity
import Data.Functor
import Control.Applicative
import Data.Semigroup
import Prelude (succ)
import Prelude (Int)

import qualified Data.List as List

import Test.Tasty
import Test.Tasty.HUnit ((@?=), testCase)

import SupplyChain.Core

tests :: TestTree
tests = testGroup "Core"
    [ testGroup "Pure _"
        [ testCase "run Identity" $ runJob (Pure 'a') () @?= Identity 'a'
        , testCase "run Maybe" $ runJob (Pure 'a') () @?= Just 'a'
        , testCase "eval" $ evalJob (Pure 'a') () @?= 'a'
        ]
    , testGroup "Pure _ <&> _"
        [ testCase "run Identity" $ runJob (Pure 'a' <&> succ) () @?= Identity 'b'
        , testCase "run Maybe" $ runJob (Pure 'a' <&> succ) () @?= Just 'b'
        , testCase "eval" $ evalJob (Pure 'a' <&> succ) () @?= 'b'
        ]
    , testGroup "Ask Pure"
        [ testCase "run Identity" $ runJob (Ask Pure) 'a' @?= Identity 'a'
        , testCase "run Maybe" $ runJob (Ask Pure) 'a' @?= Just 'a'
        , testCase "eval" $ evalJob (Ask Pure) 'a' @?= 'a'
        ]
    , testGroup "Ask Pure <&> _"
        [ testCase "run Identity" $ runJob (Ask Pure <&> succ) 'a' @?= Identity 'b'
        , testCase "run Maybe" $ runJob (Ask Pure <&> succ) 'a' @?= Just 'b'
        , testCase "eval" $ evalJob (Ask Pure <&> succ) 'a' @?= 'b'
        ]
    , testGroup "Ask Pure & contramapJob _"
        [ testCase "run Identity" $ runJob (Ask Pure & contramapJob succ) 'a' @?= Identity 'b'
        , testCase "run Maybe" $ runJob (Ask Pure & contramapJob succ) 'a' @?= Just 'b'
        , testCase "eval" $ evalJob (Ask Pure & contramapJob succ) 'a' @?= 'b'
        ]
    , testGroup "Effect"
        [ testCase "Single" $ runJob (Effect (Perform ['a', 'b'])) () @?= ['a', 'b']
        , testCase "Functor" $ runJob (Effect (Perform ['a', 'b']) <&> succ) () @?= ['b', 'c']
        , testCase "Applicative composition" $ runJob
            ( (<>) <$> Effect (Perform ["a", "b"])
                   <*> Effect (Perform ["c", "d"])
            ) () @?= ["ac", "ad", "bc", "bd"]
        , testCase "Monadic composition" $ runJob
            do { a <- Effect (Perform [1 :: Int, 3])
               ; b <- Effect (Perform ['a', 'b', 'c'])
               ; Effect (Perform (List.replicate a b))
               } () @?= "abcaaabbbccc"
        ]
    , testGroup "Request"
        let
          -- Converts dynamic effects to static effects
          f = vendorToJob go where go = Vendor \x -> Effect (Perform x) <&> \y -> Supply y go
        in
        [ testCase "Single" $ runJob (f $ Effect (Request ['a', 'b'])) () @?= ['a', 'b']
        , testCase "Functor" $ runJob (f $ Effect (Request ['a', 'b']) <&> succ) () @?= ['b', 'c']
        -- , testCase "Applicative composition" $ runJob
        --     ( f $ (<>) <$> Effect (Request ["a", "b"])
        --                <*> Effect (Request ["c", "d"])
        --     ) () @?= ["ac", "ad", "bc", "bd"]
        -- , testCase "Monadic composition" $ runJob
        --     (f $ do { a <- Effect (Request [1 :: Int, 3])
        --             ; b <- Effect (Request ['a', 'b', 'c'])
        --             ; Effect (Request (List.replicate a b))
        --             }) () @?= "abcaaabbbccc"
        ]
    ]
