module Block.ListLike.Type
  (
    NonEmptyListLike (..),
  )
  where

import Integer (Positive)

data NonEmptyListLike c = NonEmptyListLike
    { generalize :: !c
    , length :: !Positive
    }
