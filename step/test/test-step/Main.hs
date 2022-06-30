module Main (main) where

import Step.Internal.Prelude

import Hedgehog

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified ListT
import qualified ListLike
import qualified Char

import qualified Step.Document.Parser as Doc
import qualified Step.Document.Prelude as Doc

import Step.Test.InputChunking (genChunks)

import Test.Hspec
import Test.Hspec.Hedgehog

import Text (Text)

import Loc (loc)
import qualified SpanOrLoc

main :: IO ()
main = hspec do
    documentParsingSpec
