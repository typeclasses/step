module Main (main) where

import Prelude

import Test.Tasty

import SupplyChain.Spec (tests)

main :: IO ()
main = defaultMain tests
