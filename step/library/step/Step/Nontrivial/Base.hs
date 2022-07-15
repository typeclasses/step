{-# language Trustworthy #-}

module Step.Nontrivial.Base
  (
    {- * The type -} Nontrivial,
    {- * Construct and deconstruct -} refine, generalize,
    {- * List operations -} stripPrefix, length, uncons, drop,
  )
  where

import Step.Internal.Prelude hiding (uncons)

import Step.Nontrivial.Constructor

import qualified ListLike

refine :: ListLike text char => text -> Maybe (Nontrivial text)
refine x = if ListLike.null x then Nothing else Just (NontrivialUnsafe x)

generalize :: Nontrivial text -> text
generalize (NontrivialUnsafe x) = x

stripPrefix :: Eq a => ListLike text a => Nontrivial text -> Nontrivial text -> Maybe text
stripPrefix x y = ListLike.stripPrefix (generalize x) (generalize y)

length :: ListLike text a => Nontrivial text -> Natural
length = fromIntegral . ListLike.length . generalize

uncons :: ListLike text a => Nontrivial text -> (a, text)
uncons x = case ListLike.uncons (generalize x) of
    Nothing -> error "trivial Nontrivial"
    Just y -> y

drop :: ListLike text a => Natural -> Nontrivial text -> text
drop n = ListLike.drop (fromIntegral n) . generalize
