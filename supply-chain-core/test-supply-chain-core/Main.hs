module Main (main) where

import Prelude (Int, succ)
import System.IO (IO)
import Data.Functor.Identity (Identity (Identity))
import Data.Functor ((<&>), (<$>))
import Data.Semigroup ((<>))
import Control.Applicative (pure, (<*>))
import Data.Function (($))
import Data.Maybe (Maybe (..))
import Data.Functor.Const (Const)
import Data.Void (Void)
import Data.Char (Char)

import qualified Data.List as List

import Test.Tasty
import Test.Tasty.HUnit ((@?=), testCase)

import SupplyChain.Core.Connect ((>+>))
import SupplyChain.Core.Job (order, perform)
import SupplyChain.Core.Referral (Referral (Referral))
import SupplyChain.Core.Vendor (Vendor (Vendor))
import SupplyChain.Core.Unit
import qualified SupplyChain.Core.Vendor as Vendor
import qualified SupplyChain.Core.Job as Job

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Core"
    [ testGroup "pure _" $
        let
          v :: Vendor (Const Void) (Unit Char) action
          v = Vendor.unit (pure 'a')
        in
          [ testCase "run Identity" $ Vendor.run v Unit @?= Identity 'a'
          , testCase "run Maybe" $ Vendor.run v Unit @?= Just 'a'
          , testCase "eval" $ Job.eval v Unit @?= 'a'
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
          f = (go >+>) where go = Vendor \x -> perform x <&> (`Referral` go)
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
