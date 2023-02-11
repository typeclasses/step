module Block.Class.BiPrefix.Types where

data Which = First | Second

data BiPrefix xs =
    Same
  | NoPrefixRelation
  | IsPrefix Which xs xs
