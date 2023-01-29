module Cursor.Decode.Type where

import Data.Sequence (Seq)
import Block.Sequence (Seq1)

newtype Decode block1 block2 =
    Decode (Seq1 block1 -> DecodeResult block1 block2)

data DecodeResult block1 block2 = DecodeResult
    { decoded :: Seq block2
    , remainder :: Seq block1
    , liveness :: Liveness
    }

data Liveness = Dead | Alive
