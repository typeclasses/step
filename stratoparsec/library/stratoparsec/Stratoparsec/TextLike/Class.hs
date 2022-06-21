module Stratoparsec.TextLike.Class where

import qualified Char as C
import qualified Text as T
import qualified LText as LT

import Stratoparsec.Text.Buffer

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
