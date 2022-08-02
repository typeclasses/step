{-# language FlexibleContexts, Trustworthy #-}

module Step.Nontrivial.ListLike
  (
    untrivializeOperation,
    spanOperation,
    dropOperation,
    whileOperation,
    splitOperation,
  )
  where

import Step.Internal.Prelude

import Step.Nontrivial.Operations

import Step.Nontrivial.ListLike.Construction

import Step.Nontrivial.Type

import qualified ListLike

import qualified Positive
import qualified Positive.Unsafe as Positive
import qualified Positive.Math as Positive
import qualified Signed

spanOperation :: ListLike xs x => SpanOperation xs x
spanOperation = SpanOperation \f whole ->
    tupleSpan (ListLike.span (getPredicate f) (generalize whole))
  where
    tupleSpan (a, b) =
        if ListLike.null b then SpanAll else
        if ListLike.null a then SpanNone else
        SpanPart (nontrivialUnsafe a) (nontrivialUnsafe b)

dropOperation :: ListLike xs x => DropOperation xs x
dropOperation = DropOperation \n whole ->
    case Positive.minus (length whole) n of
        Signed.Zero ->
            DropAll
        Signed.Plus _ ->
            DropPart
              { dropRemainder = nontrivialUnsafe $
                  ListLike.drop (fromIntegral (review Positive.refine n)) (generalize whole)
              }
        Signed.Minus dropShortfall ->
            DropInsufficient{ dropShortfall }

whileOperation :: ListLike xs x => WhileOperation xs x
whileOperation = WhileOperation \f x ->
    case untrivialize untrivializeOperation
          (ListLike.takeWhile (getPredicate f) (generalize x))
      of
        Nothing -> WhileNone
        Just y ->
            if length y == length x
            then WhileAll
            else WhilePrefix y

splitOperation :: ListLike xs x => SplitOperation xs x
splitOperation = SplitOperation \n whole ->
    case Positive.minus (length whole) n of
        Signed.Minus n' -> SplitInsufficient{ splitShortfall = n' }
        Signed.Zero -> SplitAll
        Signed.Plus _ ->
            let
                (a, b) = ListLike.splitAt
                    (fromIntegral (review Positive.refine n))
                    (generalize whole)
            in Split (nontrivialUnsafe a) (nontrivialUnsafe b)
