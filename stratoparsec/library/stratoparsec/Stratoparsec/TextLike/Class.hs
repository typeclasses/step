module Stratoparsec.TextLike.Class where

class TextLike text where

    type Char text :: Type

    type Lazy text :: Type

    type Buffer text :: Type

    textToBuffer :: text -> Buffer text

    bufferConcat :: Buffer text -> Buffer text -> Buffer text

    bufferToLazy :: Buffer text -> Lazy text

    bufferSize :: Buffer text -> Natural
