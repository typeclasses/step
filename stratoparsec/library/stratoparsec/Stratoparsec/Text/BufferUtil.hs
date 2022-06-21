module Stratoparsec.Text.BufferUtil where

import qualified Text as T
import qualified LText as LT
import qualified Seq

import qualified Stratoparsec.Text.BufferType as TextBuffer
import Stratoparsec.Text.BufferType (TextBuffer (TextBuffer))

fromText :: Text -> TextBuffer
fromText x =
  TextBuffer{
    TextBuffer.texts = Seq.singleton x,
    TextBuffer.length = fromIntegral (T.length x)
  }

concat :: TextBuffer -> TextBuffer -> TextBuffer
concat a b =
  TextBuffer{
    TextBuffer.texts = TextBuffer.texts a <> TextBuffer.texts b,
    TextBuffer.length = TextBuffer.length a + TextBuffer.length b
  }

toLazyText :: TextBuffer -> LT.Text
toLazyText x = LT.fromChunks (toList (TextBuffer.texts x))

length :: TextBuffer -> Natural
length = TextBuffer.length
