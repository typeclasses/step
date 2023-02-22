module Main (main) where

import Essentials
import Block.Class

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
    describe "ByteString1" $ Block.spec genByte genByteString1
    describe "Text1" $ Block.spec genChar genText1
    describe "Seq1" $ Block.spec genChar (genSeq1 genChar)
    describe "BlockBlock" $ Block.spec genText1 (genSeq1 genText1)

genByte :: Gen Word8
genByte = Gen.enumBounded

genChar :: Gen Char
genChar = Gen.alphaNum

genByteString1 :: Gen ByteString1
genByteString1 = Gen.bytes (Range.linear 1 20) <&> assume

genText1 :: Gen Text1
genText1 = Gen.text (Range.linear 1 20) genChar <&> assume

genSeq1 :: Gen a -> Gen (Seq1 a)
genSeq1 g = Gen.seq (Range.linear 1 20) g <&> assume
