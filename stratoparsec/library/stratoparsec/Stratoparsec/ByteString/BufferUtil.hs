module Stratoparsec.ByteString.BufferUtil where

import qualified ByteString as BS
import qualified LByteString as LBS
import qualified Seq

import qualified Stratoparsec.ByteString.BufferType as ByteStringBuffer
import Stratoparsec.ByteString.BufferType (ByteStringBuffer (ByteStringBuffer))

fromByteString :: ByteString -> ByteStringBuffer
fromByteString x =
  ByteStringBuffer{
    ByteStringBuffer.texts = Seq.singleton x,
    ByteStringBuffer.length = fromIntegral (BS.length x)
  }

concat :: ByteStringBuffer -> ByteStringBuffer -> ByteStringBuffer
concat a b =
  ByteStringBuffer{
    ByteStringBuffer.texts = ByteStringBuffer.texts a <> ByteStringBuffer.texts b,
    ByteStringBuffer.length = ByteStringBuffer.length a + ByteStringBuffer.length b
  }

toLazyByteString :: ByteStringBuffer -> LBS.ByteString
toLazyByteString x = LBS.fromChunks (toList (ByteStringBuffer.texts x))

length :: ByteStringBuffer -> Natural
length = ByteStringBuffer.length
