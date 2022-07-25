module Step.Nontrivial
  (
    {- * The type -} Nontrivial,
    {- * Refinement -} refine, generalize,
    {- * Operations -} stripPrefix, uncons, drop, head, fold, isPrefixOf,
    {- ** length -} length, lengthNat, lengthInt,
    {- ** span -} span, Span,
    {- ** splitAt -} splitAt, SplitAt,
    {- ** splitAtPositive -} splitAtPositive, SplitAtPositive,
    {- ** takeWhile -} takeWhile, TakeWhile,
  ) where

import Step.Internal.Prelude hiding (uncons, fold)

import Step.Nontrivial.Unsafe
import Step.Nontrivial.Refinement
import Step.Nontrivial.Length
import Step.Nontrivial.Span
import Step.Nontrivial.SplitAt
import Step.Nontrivial.SplitAtPositive
import Step.Nontrivial.TakeWhile

import qualified ListLike
import qualified Maybe
import qualified Positive

stripPrefix :: Eq x => ListLike xs x => Nontrivial xs x -> Nontrivial xs x -> Maybe xs
stripPrefix a b = ListLike.stripPrefix (generalize a) (generalize b)

uncons :: ListLike xs x => Nontrivial xs x -> (x, xs)
uncons a = case ListLike.uncons (generalize a) of
    Nothing -> error "trivial Nontrivial"
    Just b -> b

drop :: ListLike xs x => Natural -> Nontrivial xs x -> xs
drop n = ListLike.drop (fromIntegral n) . generalize

head :: ListLike xs x => Nontrivial xs x -> x
head = ListLike.head . generalize

fold :: ListLike xs x => [Nontrivial xs x] -> xs
fold = ListLike.foldMap generalize

isPrefixOf :: ListLike xs x => Eq x => Nontrivial xs x -> Nontrivial xs x -> Bool
a `isPrefixOf` b = generalize a `ListLike.isPrefixOf` generalize b
