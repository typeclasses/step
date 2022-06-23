module Stratoparsec where

import Char (Char)
import Word (Word8)
import ListT (ListT)
import Seq (Seq)

import qualified ByteString as BS
import qualified LByteString as LBS
import qualified Text as T
import qualified LText as LT
import qualified Seq

class TextLike text where
    type TextLikeChar text :: Type
    type TextLikeLazy text :: Type
    type TextLikeBuffer text :: Type
    textLikeToBuffer :: text -> TextLikeBuffer text
    textLikeBufferConcat :: TextLikeBuffer text -> TextLikeBuffer text -> TextLikeBuffer text
    textLikeBufferToLazy :: TextLikeBuffer text -> TextLikeLazy text
    textLikeBufferSize :: TextLikeBuffer text -> Natural

instance TextLike T.Text
  where
    type TextLikeChar T.Text = Char
    type TextLikeLazy T.Text = LT.Text
    type TextLikeBuffer T.Text = TextBuffer
    textLikeToBuffer x =
        TextBuffer{
            textBufferTexts = Seq.singleton x,
            textBufferLength = fromIntegral (T.length x)
        }
    textLikeBufferConcat a b =
        TextBuffer{
            textBufferTexts = textBufferTexts a <> textBufferTexts b,
            textBufferLength = textBufferLength a + textBufferLength b
        }
    textLikeBufferToLazy x = LT.fromChunks (toList (textBufferTexts x))
    textLikeBufferSize = textBufferLength

instance TextLike BS.ByteString
  where
    type TextLikeChar BS.ByteString = Word8
    type TextLikeLazy BS.ByteString = LBS.ByteString
    type TextLikeBuffer BS.ByteString = ByteStringBuffer
    textLikeToBuffer x =
        ByteStringBuffer{
            byteStringBufferTexts = Seq.singleton x,
            byteStringBufferLength = fromIntegral (BS.length x)
        }
    textLikeBufferConcat  a b =
        ByteStringBuffer{
            byteStringBufferTexts = byteStringBufferTexts a <> byteStringBufferTexts b,
            byteStringBufferLength = byteStringBufferLength a + byteStringBufferLength b
        }
    textLikeBufferToLazy x = LBS.fromChunks (toList (byteStringBufferTexts x))
    textLikeBufferSize = byteStringBufferLength

type BufferedStream :: (Type -> Type) -> Type -> Type -> Type
data BufferedStream m chunk buffer =
  ParseState
    { bufferedStreamBuffer :: buffer
    , bufferedStreamPending :: ListT m chunk
    }

data ByteStringBuffer =
  ByteStringBuffer
    { byteStringBufferTexts :: Seq ByteString
    , byteStringBufferLength :: Natural
    }

data TextBuffer =
  TextBuffer
    { textBufferTexts :: Seq Text
    , textBufferLength :: Natural
    }

-- | Force the input until at least @n@ characters of input are buffered or the end of input is reached.
bufferCharacters :: Natural
    -> BufferedStream m text (TextLikeBuffer text)
    -> m (BufferedStream m text (TextLikeBuffer text))
bufferCharacters n s = _

-- bufferAtLeast :: forall m text. (Monad m, TextLike text) => Natural -> Parser m text ()
-- bufferAtLeast n = bufferSize >>= \si -> when (si < n) $ T.Parser \_ -> do
--     s <- get
--     lift $ ListT.next (BufferedStream.pending s) >>= _


-- ensure n = T.Parser $ \t pos more lose succ ->
--     case lengthAtLeast pos n t of
--       Just n' -> succ t pos more (n', substring pos n' t)
--       -- The uncommon case is kept out-of-line to reduce code size:
--       Nothing -> ensureSuspended n t pos more lose succ

-- | The parser @satisfy p@ succeeds for any character for which the predicate @p@ returns 'True'
--
-- Returns the character that is actually parsed.
-- satisfy :: TextLike text => (Char text -> Bool) -> Parser m text (Char text)
-- satisfy = _
