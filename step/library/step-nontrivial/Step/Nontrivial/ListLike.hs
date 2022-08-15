{-# language FlexibleContexts, Trustworthy #-}

module Step.Nontrivial.ListLike
  (
    untrivializeOperation,
    spanOperation,
    dropOperation,
    whileOperation,
    splitOperation,
    leftViewOperation,
  )
  where

import Step.Internal.Prelude

import Step.Nontrivial.Operations

import Step.Nontrivial.ListLike.Construction

import Step.Nontrivial.Type
import Step.Nontrivial.Unsafe

import qualified ListLike

import Positive.Unsafe
import qualified Positive
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
        Signed.Plus _ -> Split (nontrivialUnsafe a) (nontrivialUnsafe b)
          where
            (a, b) = ListLike.splitAt
                (fromIntegral (review Positive.refine n))
                (generalize whole)
        _ -> SplitInsufficient

leftViewOperation :: forall xs x. ListLike xs x => LeftViewOperation xs x
leftViewOperation = LeftViewOperation{ leftView = iso f g }
  where
    f :: Nontrivial xs x -> Pop xs x
    f a = a
        & generalize
        & ListLike.uncons
        & fromMaybe (error "ListLike leftViewIso")
        & \(x, b) -> Pop
            { popItem = x
            , popRemainder =
                case Positive.minus (length a) (PositiveUnsafe 1) of
                    Signed.Plus n -> Just NontrivialUnsafe
                        { generalize = b
                        , length = n
                        }
                    _ -> Nothing
            }

    g :: Pop xs x -> Nontrivial xs x
    g Pop{ popItem, popRemainder } = case popRemainder of
        Nothing -> NontrivialUnsafe
          { generalize = ListLike.singleton popItem
          , length = PositiveUnsafe 1
          }
        Just b -> NontrivialUnsafe
          { generalize = ListLike.cons popItem (generalize b)
          , length = Positive.plus (length b) (PositiveUnsafe 1)
          }
