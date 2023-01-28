module Cursor.Decode.Type where



newtype Decode block1 block2 =
    Decode ([block1] -> DecodeResult block1 block2)

data DecodeResult block1 block2
