module Stratoparsec.ByteString.Buffer where

data ByteStringBuffer =
  ByteStringBuffer
    { texts :: Seq ByteString
    , length :: Natural
    }
