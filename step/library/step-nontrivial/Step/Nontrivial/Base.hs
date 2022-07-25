{-# language Trustworthy #-}

module Step.Nontrivial.Base
  (
    {- * The type -} Nontrivial,
    {- * Construct and deconstruct -} refine, generalize,
    {- * List operations -} stripPrefix, uncons, drop, head, fold, isPrefixOf,
  )
  where

import Step.Internal.Prelude hiding (uncons, fold)

import Step.Nontrivial.Unsafe

import qualified ListLike

refine :: ListLike xs x => xs -> Maybe (Nontrivial xs x)
refine x = if ListLike.null x then Nothing else Just (NontrivialUnsafe x)

generalize :: Nontrivial xs x -> xs
generalize (NontrivialUnsafe xs) = xs

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
