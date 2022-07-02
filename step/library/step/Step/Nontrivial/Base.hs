module Step.Nontrivial.Base
  (
    {- * The type -} Nontrivial,
    {- * Construct and deconstruct -} refine, generalize,
    {- * List operations -} stripPrefix, length, uncons,
  )
  where

import Step.Internal.Prelude hiding (uncons)

import qualified ListLike

newtype Nontrivial a = Nontrivial a
    deriving newtype (Semigroup, Eq, Ord, Show)

refine :: ListLike text char => text -> Maybe (Nontrivial text)
refine x = if ListLike.null x then Nothing else Just (Nontrivial x)

generalize :: Nontrivial text -> text
generalize (Nontrivial x) = x

stripPrefix :: Eq a => ListLike text a => Nontrivial text -> Nontrivial text -> Maybe text
stripPrefix x y = ListLike.stripPrefix (generalize x) (generalize y)

length :: ListLike text a => Nontrivial text -> Natural
length = fromIntegral . ListLike.length . generalize

uncons :: ListLike text a => Nontrivial text -> (a, text)
uncons x = case ListLike.uncons (generalize x) of
    Nothing -> error "trivial Nontrivial"
    Just y -> y
