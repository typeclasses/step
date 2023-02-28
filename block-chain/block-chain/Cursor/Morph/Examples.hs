module Cursor.Morph.Examples
  (
    {- * Transformations -} decodeAscii,
  )
  where

import Essentials
import Cursor.Morph.Type
import Cursor.Interface
import Cursor.Decode.Type

import Block (ASCII1)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Word (Word8)
import SupplyChain (Vendor (..))

import qualified ASCII.Char as ASCII

decodeAscii :: MorphPlus up action 'Write Word8 ByteString ASCII.Char ASCII1
decodeAscii = Vendor \case
    Next -> _
    Commit n -> _
    Reset -> _
    Flush -> _
