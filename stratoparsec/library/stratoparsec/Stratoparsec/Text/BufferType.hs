module Stratoparsec.Text.BufferType where

data TextBuffer =
  TextBuffer
    { texts :: Seq Text
    , length :: Natural
    }
