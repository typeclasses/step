module Block.BiPrefix.Types where

data BiPrefix xs =
    Same
  | NoPrefixRelation
  | IsPrefixOf   { commonPart :: xs, extraPart :: xs }
  | IsPrefixedBy { commonPart :: xs, extraPart :: xs }
