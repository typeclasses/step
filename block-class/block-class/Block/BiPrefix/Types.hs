module Block.BiPrefix.Types where

import Block.Positional.Types

data Which = First | Second

data BiPrefix xs =
    Same
  | NoPrefixRelation
  | IsPrefix Which (Prefix xs, Suffix xs)
