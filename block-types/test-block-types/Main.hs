module Main (main) where

import Essentials
import Block.Class

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Sequence (Seq)
import Block.ByteString (ByteString1)
import Block.Sequence (Seq1)
import Block.Text (Text1)
import Data.Char (Char)
import Data.Word (Word8)
import Hedgehog (Gen)
import System.IO (IO)
import Test.Hspec (hspec, describe)

import qualified Block.Hedgehog.Spec as Block
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

main :: IO ()
main = hspec do

    describe "ByteString1" $
        Block.refinedSpec genByte genByteString genByteString1

    describe "Text1" $
        Block.refinedSpec genChar genText genText1

    describe "Seq1" $
        Block.refinedSpec genChar (genSeq genChar) (genSeq1 genChar)

    describe "BlockBlock" $
        Block.spec genText1 (genSeq1 genText1)

genByte :: Gen Word8
genByte = Gen.enumBounded

genChar :: Gen Char
genChar = Gen.alphaNum

genByteString :: Gen ByteString
genByteString = Gen.bytes (Range.linear 0 20)

genByteString1 :: Gen ByteString1
genByteString1 = Gen.bytes (Range.linear 1 20) <&> assume

genText :: Gen Text
genText = Gen.text (Range.linear 0 20) genChar

genText1 :: Gen Text1
genText1 = Gen.text (Range.linear 1 20) genChar <&> assume

genSeq :: Gen a -> Gen (Seq a)
genSeq g = Gen.seq (Range.linear 0 20) g

genSeq1 :: Gen a -> Gen (Seq1 a)
genSeq1 g = Gen.seq (Range.linear 1 20) g <&> assume
