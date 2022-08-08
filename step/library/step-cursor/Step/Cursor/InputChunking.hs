{-# language FlexibleContexts, ViewPatterns #-}

module Step.Cursor.InputChunking
  (
    genChunks,
  )
  where

import Step.Internal.Prelude

import Hedgehog

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified ListLike

import qualified Step.Nontrivial.ListLike as Nontrivial.ListLike
import Step.Nontrivial (Nontrivial)
import qualified Step.Nontrivial as Nontrivial
import Step.Nontrivial.ListLike
import Step.Nontrivial.Operations

import qualified Maybe

import qualified Seq

import qualified Positive

genChunks :: ListLike xs x => xs -> Gen [Nontrivial xs x]
genChunks x = case untrivialize x of
    Nothing -> return []
    Just y -> fmap ListLike.toList $ genChunks' y
  where
    UntrivializeOperation{ untrivialize } = Nontrivial.ListLike.untrivializeOperation

genChunks' :: ListLike xs x => Nontrivial xs x -> Gen (Seq (Nontrivial xs x))
genChunks' x = Gen.recursive Gen.choice [return (Seq.singleton x)] [genChunks'' x]

genChunks'' :: ListLike xs x => Nontrivial xs x -> Gen (Seq (Nontrivial xs x))
genChunks'' x = if Nontrivial.lengthNat x == 1 then return (Seq.singleton x) else do
    Just i <- preview Positive.refine <$> Gen.integral (Range.constant 1 (Nontrivial.lengthNat x - 1))
    case split i x of
        SplitInsufficient -> error "genChunks: SplitInsufficient"
        Split a b -> (<>) <$> genChunks' a <*> genChunks' b
  where
    SplitOperation{ split } = Nontrivial.ListLike.splitOperation
