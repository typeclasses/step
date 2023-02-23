module Main (main) where

import Essentials
import Block.Class

import Block.BlockBlock (BlockBlock (..))
import Block.ByteString (ByteString1)
import Block.Hedgehog.Spec (PredicateGenerators (..))
import Block.Sequence (Seq1)
import Block.Text (Text1)
import Data.ByteString (ByteString)
import Data.Char (Char)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Word (Word8)
import Hedgehog (Gen)
import Data.List.NonEmpty (NonEmpty)
import System.IO (IO)
import Test.Hspec (hspec, describe)

import qualified Block.Hedgehog.Spec as Block
import qualified Data.ByteString as ByteString
import qualified Data.Char as Char
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

main :: IO ()
main = hspec do

    describe "ByteString1" $
        Block.refinedSpec
            genByte
            genByteString
            genByteString1
            genByteStringPredicate

    describe "Text1" $
        Block.refinedSpec
            genChar
            genText
            genText1
            genTextPredicate

    describe "Seq1" $
        Block.refinedSpec
            genChar
            (genSeq genChar)
            (genSeq1 genChar)
            genCharSeqPredicate

    describe "BlockBlock" $
        Block.spec @Char @(BlockBlock Char Text1 (Seq1 Text1))
            genChar
            (genSeq1 genText1 <&> BlockBlock)
            (genBlockBlockPredicate genTextPredicate)

    describe "NonEmpty" $
        Block.spec @Char @(NonEmpty Char)
            genChar
            (genNonEmpty genChar)
            genCharNonEmptyPredicate

genByte :: Gen Word8
genByte = Gen.enumBounded

genChar :: Gen Char
genChar = Gen.alphaNum

genByteString :: Gen ByteString
genByteString = Gen.bytes (Range.linear 0 10)

genByteString1 :: Gen ByteString1
genByteString1 = Gen.bytes (Range.linear 1 10) <&> assume

genText :: Gen Text
genText = Gen.text (Range.linear 0 10) genChar

genText1 :: Gen Text1
genText1 = Gen.text (Range.linear 1 10) genChar <&> assume

genSeq :: Gen a -> Gen (Seq a)
genSeq g = Gen.seq (Range.linear 0 10) g

genSeq1 :: Gen a -> Gen (Seq1 a)
genSeq1 g = Gen.seq (Range.linear 1 10) g <&> assume

genNonEmpty :: Gen a -> Gen (NonEmpty a)
genNonEmpty g = Gen.nonEmpty (Range.linear 1 10) g

genByteStringPredicate :: PredicateGenerators Word8 ByteString1
genByteStringPredicate = PredicateGenerators (>= 128) genX genXs
  where
    genX t = Gen.integral case t of
        False -> Range.linear 0 127
        True -> Range.linear 255 128
    genXs t = Gen.list (Range.linear 1 10) (genX t)
        <&> (ByteString.pack >>> assume)

genTextPredicate :: PredicateGenerators Char Text1
genTextPredicate = PredicateGenerators Char.isUpper genX genXs
  where
    genX = \case False -> Gen.lower; True -> Gen.upper
    genXs t = Gen.text (Range.linear 1 10) (genX t) <&> assume

genCharSeqPredicate :: PredicateGenerators Char (Seq1 Char)
genCharSeqPredicate = PredicateGenerators Char.isUpper genX genXs
  where
    genX = \case False -> Gen.lower; True -> Gen.upper
    genXs t = Gen.seq (Range.linear 1 10) (genX t) <&> assume

genCharNonEmptyPredicate :: PredicateGenerators Char (NonEmpty Char)
genCharNonEmptyPredicate = PredicateGenerators Char.isUpper genX genXs
  where
    genX = \case False -> Gen.lower; True -> Gen.upper
    genXs t = Gen.nonEmpty (Range.linear 1 10) (genX t)

genBlockBlockPredicate :: Positional xs =>
    PredicateGenerators x xs
    -> PredicateGenerators x (BlockBlock x xs (Seq1 xs))
genBlockBlockPredicate (PredicateGenerators p genX genXs) =
    PredicateGenerators p genX genXss
  where
    genXss t = Gen.seq (Range.linear 1 10) (genXs t)
        <&> (assume >>> BlockBlock)
