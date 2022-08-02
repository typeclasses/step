{-# language BangPatterns, GeneralizedNewtypeDeriving, Unsafe #-}

module Step.Nontrivial.Constructor
  (
    {- * Type -} Nontrivial (..),
    {- * Creation -} refine, nontrivialUnsafe,
    {- * Operation result types -} Span (..),
  )
  where

import Step.Internal.Prelude

import qualified ListLike

import qualified Positive
import qualified Positive.Unsafe as Positive
import qualified Positive.Math as Positive
import qualified Signed

data Nontrivial xs x =
  NontrivialUnsafe
    { generalize :: !xs
    , length :: Positive Natural
    , head :: x
    , tail :: Maybe (Nontrivial xs x)

    , span :: Predicate x -> Span xs x
    }

data Span xs x =
    SpanAll
  | SpanNone
  | SpanPart{ spannedPart :: Nontrivial xs x, spanRemainder :: Nontrivial xs x }

nontrivialUnsafe :: ListLike xs x => xs -> Nontrivial xs x
nontrivialUnsafe x = fix \nt ->
    NontrivialUnsafe
      { generalize = x
      , length = (Positive.PositiveUnsafe . fromIntegral . ListLike.length) x
      , head = ListLike.head x
      , tail = refine (ListLike.tail x)
      , span = span' nt
      }

refine :: ListLike xs x => xs -> Maybe (Nontrivial xs x)
refine x =
    (preview Positive.natPrism . fromIntegral . ListLike.length) x <&>
    \l -> fix \nt ->
      NontrivialUnsafe
        { generalize = x
        , length = l
        , head = ListLike.head x
        , tail = refine (ListLike.tail x)
        , span = span' nt
        }

span' :: ListLike xs x => Nontrivial xs x -> Predicate x -> Span xs x
span' whole f = tupleSpan (ListLike.span (getPredicate f) (generalize whole))

tupleSpan :: ListLike xs x => (xs, xs) -> Span xs x
tupleSpan (a, b) =
    if ListLike.null b then SpanAll else
    if ListLike.null a then SpanNone else
    SpanPart (nontrivialUnsafe a) (nontrivialUnsafe b)
