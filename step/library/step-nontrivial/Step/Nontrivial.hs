module Step.Nontrivial
  (
    {- * The type -} Nontrivial,
    {- * Refinement -} refine, generalize,
    {- * Operations -} stripPrefix, uncons, head, fold, isPrefixOf,
    {- ** length -} length, lengthNat, lengthInt,
    {- ** span -} span, Span,
    {- ** splitAt -} splitAt, SplitAt, splitAtPositive, SplitAtPositive,
    {- ** takeWhile -} takeWhile, TakeWhile,
    {- ** drop -} drop, dropPositive, Drop,
  ) where

import Step.Internal.Prelude hiding (uncons, fold)

import Step.Nontrivial.Drop
import Step.Nontrivial.Length
import Step.Nontrivial.Refinement
import Step.Nontrivial.Span
import Step.Nontrivial.SplitAt
import Step.Nontrivial.SplitAtPositive
import Step.Nontrivial.TakeWhile
import Step.Nontrivial.Unsafe

import qualified ListLike
import qualified Maybe
import qualified Positive

stripPrefix :: Eq x => ListLike xs x => Nontrivial xs x -> Nontrivial xs x -> Maybe xs
stripPrefix a b = ListLike.stripPrefix (generalize a) (generalize b)

uncons :: ListLike xs x => Nontrivial xs x -> (x, xs)
uncons a = case ListLike.uncons (generalize a) of
    Nothing -> error "trivial Nontrivial"
    Just b -> b

head :: ListLike xs x => Nontrivial xs x -> x
head = ListLike.head . generalize

fold :: ListLike xs x => [Nontrivial xs x] -> xs
fold = ListLike.foldMap generalize

isPrefixOf :: ListLike xs x => Eq x => Nontrivial xs x -> Nontrivial xs x -> Bool
a `isPrefixOf` b = generalize a `ListLike.isPrefixOf` generalize b
