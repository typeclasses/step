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

    describe "takePositive" do
        it "takes some fixed amount of input" do
            let input :: [Text1] = ["ab", "cd", "ef"]
            readBlockList (takePositive 3) input `shouldBe`
                (AdvanceSuccess ["ab", "c"], ["d", "ef"])

    describe "decodeAscii" do

        it "turns ByteString1 into ASCII1" do
            let input :: [ByteString1] = ["ab", "cd", "ef"]
            readBlockList (morph decodeAscii $ takePositive 3) input `shouldBe`
                (AdvanceSuccess ["ab", "c"], ["d", "ef"])

        it "lets us read an ASCII prefix of a not-entirely-ASCII input" do
            let input :: [ByteString1] = [[97, 98], [99, 229], [230, 231]]
            readBlockList (morph decodeAscii $ takePositive 3) input `shouldBe`
                (AdvanceSuccess ["ab", "c"], [[229], [230, 231]])

        it "stops at the first non-ASCII byte" do
            let input :: [ByteString1] = [[97, 98], [99, 229], [230, 231]]
            readBlockList (morph decodeAscii $ takePositive 4) input `shouldBe`
                (YouCanNotAdvance (Shortfall 1) ["ab", "c"], [[229], [230, 231]])

    describe "HTTP example" do

        pure ()

        -- it "Request line" $ hedgehog do
        --     let input :: Text = "GET /hello.txt HTTP/1.1"
        --         reader = undefined -- todo
        --     chunkedInput :: [ByteString1] <- forAll $ Gen.shatter0 input
            -- runReader reader chunkedInput `shouldBe` ("GET", ["hello.txt"], ['1', '1'])
