module Stratoparsec.ByteString.BufferType where

data ByteStringBuffer =
  ByteStringBuffer
    { texts :: Seq ByteString
    , length :: Natural
    }
