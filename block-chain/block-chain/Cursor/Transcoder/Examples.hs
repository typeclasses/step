module Cursor.Transcoder.Examples
  (
    {- * Transcoders -} decodeUtf8,
  )
  where

import Essentials
import Cursor.Transcoder.Type
import Cursor.Interface

import Data.Text (Text)
import Data.ByteString (ByteString)

decodeUtf8 :: TranscoderPlus up action 'Write ByteString Text
decodeUtf8 = _
