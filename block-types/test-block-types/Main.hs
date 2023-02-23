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
import Prelude (fromIntegral)
import System.IO (IO)
import Test.Hspec (hspec, describe)

import qualified Block.Hedgehog.Gen.Shatter as Gen
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
            pure -- no way to variegate
            genByteStringPredicate

    describe "Text1" $
        Block.refinedSpec
            genChar
            genText
            genText1
            pure -- no way to variegate
            genTextPredicate

    describe "Seq1" $
        Block.refinedSpec
            genChar
            (genSeq genChar)
            (genSeq1 genChar)
            pure -- no way to variegate
            genCharSeqPredicate

    describe "BlockBlock" $
        Block.spec @Char @(BlockBlock Char Text1 (Seq1 Text1))
            genChar
            (genBlockBlock @Char @Text1 @(Seq1 Text1) genText1)
            variegateBlockBlock
            (genBlockBlockPredicate @Char @Text1 @(Seq1 Text1) genTextPredicate)

genByte :: Gen Word8
genByte = Gen.choice
    [ Gen.integral (fmap fromIntegral (Range.linear (Char.ord 'a') (Char.ord 'z')))
    , Gen.integral (fmap fromIntegral (Range.linear (Char.ord 'A') (Char.ord 'Z')))
    ]

genChar :: Gen Char
genChar = Gen.alpha

genByteString :: Gen ByteString
genByteString = Gen.list (Range.linear 0 10) genByte <&> ByteString.pack

genByteString1 :: Gen ByteString1
genByteString1 = Gen.list (Range.linear 1 10) genByte <&> (ByteString.pack >>> assume)

genText :: Gen Text
genText = Gen.text (Range.linear 0 10) genChar

genText1 :: Gen Text1
genText1 = Gen.text (Range.linear 1 10) genChar <&> assume

genSeq :: Gen a -> Gen (Seq a)
genSeq g = Gen.seq (Range.linear 0 10) g

genSeq1 :: Gen a -> Gen (Seq1 a)
genSeq1 g = Gen.seq (Range.linear 1 10) g <&> assume

genByteStringPredicate :: PredicateGenerators Word8 ByteString1
genByteStringPredicate = PredicateGenerators (< fromIntegral (Char.ord 'a')) genX genXs
  where
    genX t = Gen.integral $ fmap fromIntegral $ case t of
        True  -> Range.linear (Char.ord 'A') (Char.ord 'Z')
        False -> Range.linear (Char.ord 'a') (Char.ord 'z')
    genXs t = Gen.list (Range.linear 1 10) (genX t) <&> (ByteString.pack >>> assume)

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

genBlockBlock :: forall x xs xss.
    (Positional xs, NonEmptyIso xs xss) =>
    Gen xs -> Gen (BlockBlock x xs xss)
genBlockBlock g = do
    xs <- g
    xss <- Gen.shatter1 xs
    pure $ BlockBlock $ fromNonEmpty Front xss

genBlockBlockPredicate :: forall x xs xss.
    (Positional xs, NonEmptyIso xs xss) =>
    PredicateGenerators x xs
    -> PredicateGenerators x (BlockBlock x xs xss)
genBlockBlockPredicate (PredicateGenerators p genX genXs) =
    PredicateGenerators p genX (\t -> genBlockBlock @x @xs @xss (genXs t))

variegateBlockBlock :: (Eq x, NonEmptyIso x xs, NonEmptyIso xs xss, Singleton xs xss, Positional xs) => BlockBlock x xs xss -> Gen (BlockBlock x xs xss)
variegateBlockBlock bb = do
    let xs = toNonEmpty end bb & fromNonEmpty end
    xss <- Gen.shatter1 xs
    pure $ BlockBlock $ fromNonEmpty end xss
  where
    end = Front
