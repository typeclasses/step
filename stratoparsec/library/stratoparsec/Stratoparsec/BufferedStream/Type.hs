module Stratoparsec.BufferedStream.Type where

import ListT (ListT)

type BufferedStream :: (Type -> Type) -> Type -> Type -> Type

data BufferedStream m chunk buffer =
  ParseState
    { buffer :: buffer
    , pending :: ListT m chunk
    }
