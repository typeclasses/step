module Main (main) where

import Control.Applicative (pure, (<*>))
import Data.Function (($))
import Data.Functor ((<&>), (<$>))
import Data.Functor.Identity (Identity (Identity))
import Data.Maybe (Maybe (..))
import Data.Semigroup ((<>))
import Prelude (Int, succ)
import SupplyChain.Core.Connect ((>-))
import SupplyChain.Core.Job (order, perform)
import SupplyChain.Core.Referral (Referral (Referral))
import SupplyChain.Core.Vendor (Vendor (Vendor, handle))
import System.IO (IO)
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit ((@?=), testCase)

import qualified Data.List as List
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
        , let
            j = do
              a <- perform [1 :: Int, 3]
              b <- perform ['a', 'b', 'c']
              perform (List.replicate a b)
          in
            testCase "Monadic composition" $ Job.run j @?= "abcaaabbbccc"
        ]
    , testGroup "order" $
        let
          -- Converts dynamic effects to static effects
          f = (go >-)
            where
              go = Vendor { handle = \x -> perform x <&> (`Referral` go) }
        in
        [ testCase "Single" $ Job.run (f $ order ['a', 'b']) @?= ['a', 'b']
        , testCase "Functor" $ Job.run (f $ order ['a', 'b'] <&> succ) @?= ['b', 'c']
        , testCase "Applicative composition" $
            let
              j = f $ (<>) <$> order ["a", "b"] <*> order ["c", "d"]
            in
              Job.run j @?= ["ac", "ad", "bc", "bd"]
        , testCase "Monadic composition" $
            let
              j = do
                a <- order [1 :: Int, 3]
                b <- order ['a', 'b', 'c']
                order (List.replicate a b)
            in
              Job.run (f j) @?= "abcaaabbbccc"
        ]
    ]
