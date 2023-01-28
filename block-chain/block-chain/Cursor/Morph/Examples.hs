module Cursor.Morph.Examples
  (
    {- * Transformations -} decode,
  )
  where

import Essentials
import Cursor.Morph.Type
import Cursor.Interface
import Cursor.Decode.Type

import Data.Text (Text)
import Data.ByteString (ByteString)

decode ::
    Decode a b -- ^ See "Cursor.Decode.Examples"
    -> MorphPlus up action 'Write a b
decode = _
