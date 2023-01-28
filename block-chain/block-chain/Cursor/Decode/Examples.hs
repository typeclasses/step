module Cursor.Decode.Examples where

import Cursor.Decode.Type

import Data.ByteString (ByteString)
import Data.Text (Text)

import qualified Data.Text.Encoding as Text

hexadecimal :: Decode ByteString Text
hexadecimal = Decode _

{-

todo: a UTF-8 decoder would be great, but
https://github.com/haskell/text/issues/498

-}
