module Stratoparsec.TextLike.Parsers where

import Prelude hiding (Char)

import ListT (ListT)
import qualified ListT

import Stratoparsec.TextLike.ParserType (Parser)
import Stratoparsec.TextLike.Class (TextLike)

import qualified Stratoparsec.Parser.Type as T
import qualified Stratoparsec.TextLike.Class as TL
import qualified Stratoparsec.BufferedStream.Type as BufferedStream

amountBuffered :: forall m text. (Monad m, TextLike text) => Parser m text Natural
amountBuffered = T.Parser \_ -> get <&> TL.bufferSize @text . BufferedStream.buffer

-- | Force the input until at least @n@ characters of input are available or the end of input is reached.
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
