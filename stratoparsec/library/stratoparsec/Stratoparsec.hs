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

type BufferedStream :: (Type -> Type) -> Type -> Type -> Type
data BufferedStream m chunk buffer =
  ParseState
    { bufferedStreamBuffer :: buffer
    , bufferedStreamPending :: ListT m chunk
    }

textLikeSingletonBuffer :: MonoFoldable text => text -> Buffer text
textLikeSingletonBuffer x =
    Buffer{ bufferChunks = Seq.singleton x, bufferSize = fromIntegral $ Mono.olength x }

data Buffer a = Buffer{ bufferChunks :: Seq a, bufferSize :: Natural }

-- | Force the input until at least @n@ characters of input are buffered or the end of input is reached.
bufferCharacters :: Natural
    -> BufferedStream m text (Mono.Element text)
    -> m (BufferedStream m text (Mono.Element text))
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
