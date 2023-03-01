module Main (main) where

import Essentials
import Cursor

import Data.Text (Text)
import Test.Hspec (describe, it, shouldBe, hspec)
import Test.Hspec.Hedgehog (hedgehog)
import Hedgehog (forAll, (===))
import Block (ByteString1, Text1)
import System.IO (IO)

import qualified Block.Hedgehog.Gen.Shatter as Gen

main :: IO ()
main = hspec do

    it "takePositive" $ hedgehog do
        let input :: [Text1] = ["ab", "cd", "ef"]
        runIdentity (readBlockList (takePositive 3) input) ===
            (AdvanceSuccess ["ab", "c"], ["d", "ef"])

    describe "HTTP example" do

        pure ()

        -- it "Request line" $ hedgehog do
        --     let input :: Text = "GET /hello.txt HTTP/1.1"
        --         reader = undefined -- todo
        --     chunkedInput :: [ByteString1] <- forAll $ Gen.shatter0 input
            -- runReader reader chunkedInput `shouldBe` ("GET", ["hello.txt"], ['1', '1'])
