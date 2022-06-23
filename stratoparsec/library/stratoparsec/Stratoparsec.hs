module Stratoparsec where

import Char (Char)
import Word (Word8)
import ListT (ListT)
import Seq (Seq)
import Mono (MonoFoldable)

import qualified ByteString as BS
import qualified LByteString as LBS
import qualified Text as T
import qualified LText as LT
import qualified Seq
import qualified Mono
import qualified ListT

import Stratoparsec.Buffer (Buffer)
import qualified Stratoparsec.Buffer as Buffer

import Stratoparsec.Stream (Stream)
import qualified Stratoparsec.Stream as Stream

-- | The parser @satisfy p@ succeeds for any character for which the predicate @p@ returns 'True'
--
-- Returns the character that is actually parsed.
-- satisfy :: TextLike text => (Char text -> Bool) -> Parser m text (Char text)
-- satisfy = _
