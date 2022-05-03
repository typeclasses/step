module Stratoparsec.ByteString.ParserType where

import qualified Stratoparsec.TextLike.ParserType as T

type Parser m a = T.Parser m ByteString a
