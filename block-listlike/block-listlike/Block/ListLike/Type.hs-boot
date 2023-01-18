module Block.ListLike.Type where

import Integer (Positive)

data NonEmptyListLike c = NonEmptyListLike
    { generalize :: !c
    , length :: !Positive
    }
