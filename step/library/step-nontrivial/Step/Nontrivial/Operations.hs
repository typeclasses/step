{-# language Safe #-}

module Step.Nontrivial.Operations where

import Step.Internal.Prelude

import Step.Nontrivial.Type

newtype UntrivializeOperation xs x =
    UntrivializeOperation{ untrivialize :: xs -> Maybe (Nontrivial xs x) }

newtype SpanOperation xs x =
    SpanOperation{ span :: Predicate x -> Nontrivial xs x -> Span xs x }

data Span xs x =
    SpanAll
  | SpanNone
  | SpanPart{ spannedPart :: Nontrivial xs x, spanRemainder :: Nontrivial xs x }

newtype GeneralSpanOperation xs x =
    GeneralSpanOperation{ generalSpan :: Nontrivial xs x -> Span xs x }

generalizeSpan :: Predicate x -> SpanOperation xs x -> GeneralSpanOperation xs x
generalizeSpan p SpanOperation{ span } = GeneralSpanOperation{ generalSpan = span p }

newtype SplitOperation xs x =
    SplitOperation { split :: Positive Natural -> Nontrivial xs x -> Split xs x }

data Split xs x =
    SplitAll
  | SplitInsufficient{ splitShortfall :: Positive Natural }
  | Split (Nontrivial xs x) (Nontrivial xs x)

newtype DropOperation xs x =
    DropOperation{ drop :: Positive Natural -> Nontrivial xs x -> Drop xs x }

data Drop xs x =
    DropAll
  | DropInsufficient{ dropShortfall :: Positive Natural }
  | DropPart{ dropRemainder :: Nontrivial xs x }

-- dropNat :: ListLike xs x => Natural -> Nontrivial xs x -> Drop xs x
-- dropNat = maybe DroppedPart drop . preview Positive.refine

newtype WhileOperation xs x =
    WhileOperation{ while :: Predicate x -> Nontrivial xs x -> While xs x }

data While xs x =
    WhileNone
  | WhilePrefix (Nontrivial xs x)
  | WhileAll
