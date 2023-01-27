module Cursor.Morph.Examples
  (
    {- * Transformations -} decodeUtf8,
  )
  where

import Essentials
import Cursor.Morph.Type
import Cursor.Interface

import Data.Text (Text)
import Data.ByteString (ByteString)

decodeUtf8 :: MorphPlus up action 'Write ByteString Text
decodeUtf8 = _
