{-# language Trustworthy #-}

module Step.Nontrivial.Base
  (
    {- * The type -} Nontrivial,
    {- * Construct and deconstruct -} refine, generalize,
    {- * List operations -} stripPrefix, uncons, drop,
  )
  where

import Step.Internal.Prelude hiding (uncons)

import Step.Nontrivial.Constructor

import qualified ListLike

refine :: ListLike text char => text -> Maybe (Nontrivial text char)
refine x = if ListLike.null x then Nothing else Just (NontrivialUnsafe x)

generalize :: Nontrivial text char -> text
generalize (NontrivialUnsafe x) = x

stripPrefix :: Eq a => ListLike text a => Nontrivial text char -> Nontrivial text char -> Maybe text
stripPrefix x y = ListLike.stripPrefix (generalize x) (generalize y)

uncons :: ListLike text a => Nontrivial text char -> (a, text)
uncons x = case ListLike.uncons (generalize x) of
    Nothing -> error "trivial Nontrivial"
    Just y -> y

drop :: ListLike text a => Natural -> Nontrivial text char -> text
drop n = ListLike.drop (fromIntegral n) . generalize
