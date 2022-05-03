module Stratoparsec.Text.Buffer where

data TextBuffer =
  TextBuffer
    { texts :: Seq Text
    , length :: Natural
    }
