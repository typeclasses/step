module Cursor.Decode.Type where

import Essentials

import Block (BlockBlock)

-- newtype Decode block1 block2 =
--     Decode (BlockSequence block1 -> DecodeResult block1 block2)

-- data DecodeResult block1 block2 = DecodeResult
--     { decoded :: BlockSequence block2
--     , remainder :: Maybe (BlockSequence block1)
--     , liveness :: Liveness
--     }

-- data Liveness = Dead | Alive
