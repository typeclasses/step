module Main (main) where

import Prelude

import Test.Tasty

import Data.Functor.Identity (Identity (Identity))
import Data.Functor ((<&>))

import qualified Data.List as List

import Test.Tasty.HUnit ((@?=), testCase)

import SupplyChain.Core.Connect ( vendorToJob )
import SupplyChain.Core.Job (order, perform)
import SupplyChain.Core.Supply (Supply (Supply))
import SupplyChain.Core.Vendor (Vendor (Vendor))

import qualified SupplyChain.Core.Job as Job

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Core"
    [ testGroup "pure _"
        [ testCase "run Identity" $ Job.run (pure 'a') @?= Identity 'a'
        , testCase "run Maybe" $ Job.run (pure 'a') @?= Just 'a'
        , testCase "eval" $ Job.eval (pure 'a') @?= 'a'
        ]
    , testGroup "pure _ <&> _"
        [ testCase "run Identity" $ Job.run (pure 'a' <&> succ) @?= Identity 'b'
        , testCase "run Maybe" $ Job.run (pure 'a' <&> succ) @?= Just 'b'
        , testCase "eval" $ Job.eval (pure 'a' <&> succ) @?= 'b'
        ]
    , testGroup "perform"
        [ testCase "Single" $ Job.run (perform ['a', 'b']) @?= ['a', 'b']
        , testCase "Functor" $ Job.run (perform ['a', 'b'] <&> succ) @?= ['b', 'c']
        , testCase "Applicative composition" $
            Job.run ((<>) <$> perform ["a", "b"] <*> perform ["c", "d"])
            @?= ["ac", "ad", "bc", "bd"]
        , testCase "Monadic composition" $ Job.run
            do { a <- perform [1 :: Int, 3]
               ; b <- perform ['a', 'b', 'c']
               ; perform (List.replicate a b)
               } @?= "abcaaabbbccc"
        ]
    , testGroup "order"
        let
          -- Converts dynamic effects to static effects
          f = vendorToJob go where go = Vendor \x -> perform x <&> \y -> Supply y go
        in
        [ testCase "Single" $ Job.run (f $ order ['a', 'b']) @?= ['a', 'b']
        , testCase "Functor" $ Job.run (f $ order ['a', 'b'] <&> succ) @?= ['b', 'c']
        , testCase "Applicative composition" $ Job.run
            (f $ (<>) <$> order ["a", "b"] <*> order ["c", "d"])
            @?= ["ac", "ad", "bc", "bd"]
        , testCase "Monadic composition" $ Job.run
            (f $ do { a <- order [1 :: Int, 3]
                    ; b <- order ['a', 'b', 'c']
                    ; order (List.replicate a b)
                    }) @?= "abcaaabbbccc"
        ]
    ]
