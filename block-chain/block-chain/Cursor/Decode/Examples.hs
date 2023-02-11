module Cursor.Decode.Examples where

import Cursor.Decode.Type

import Block.Class.ByteString (ByteString1)

hexadecimalWord8 :: Decode ByteString1 ByteString1
hexadecimalWord8 = Decode \inputs ->
    _

{-

todo: a UTF-8 decoder would be great, but
https://github.com/haskell/text/issues/498

-}
