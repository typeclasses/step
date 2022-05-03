module Stratoparsec.TextLike.ParserType where

import qualified Stratoparsec.Parser.Type as T

import Stratoparsec.TextLike.Class

type Parser m text a = T.Parser m text (Buffer text) a
