module Stratoparsec.TextLike.Class where

import qualified ByteString as BS
import qualified LByteString as LBS
import qualified Char as C
import qualified Text as T
import qualified LText as LT
import qualified Word as W

import Stratoparsec.Text.BufferType
import Stratoparsec.ByteString.BufferType
import qualified Stratoparsec.Text.BufferUtil as TextBuffer
import qualified Stratoparsec.ByteString.BufferUtil as ByteStringBuffer

class TextLike text where

    type Char text :: Type

    type Lazy text :: Type

    type Buffer text :: Type

    textToBuffer :: text -> Buffer text

    bufferConcat :: Buffer text -> Buffer text -> Buffer text

    bufferToLazy :: Buffer text -> Lazy text

    bufferSize :: Buffer text -> Natural

instance TextLike T.Text
  where
    type Char T.Text = C.Char
    type Lazy T.Text = LT.Text
    type Buffer T.Text = TextBuffer
    textToBuffer = TextBuffer.fromText
    bufferConcat = TextBuffer.concat
    bufferToLazy = TextBuffer.toLazyText
    bufferSize = TextBuffer.length

instance TextLike BS.ByteString
  where
    type Char BS.ByteString = W.Word8
    type Lazy BS.ByteString = LBS.ByteString
    type Buffer BS.ByteString = ByteStringBuffer
    textToBuffer = ByteStringBuffer.fromByteString
    bufferConcat = ByteStringBuffer.concat
    bufferToLazy = ByteStringBuffer.toLazyByteString
    bufferSize = ByteStringBuffer.length
