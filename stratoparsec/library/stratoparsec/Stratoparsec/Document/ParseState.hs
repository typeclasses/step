module Stratoparsec.Document.ParseState where

import Optics

import ListT (ListT)

import Stratoparsec.Stream.Base (Stream)
import qualified Stratoparsec.Stream.Base as Stream

import Stratoparsec.Document.Past (Past)
import qualified Stratoparsec.Document.Past as Past

data ParseState m =
  ParseState
    { past :: Past
    , future :: Stream m Text
    }

makeLensesFor [("future", "futureLens"), ("past", "pastLens")] ''ParseState

start :: ListT m Text -> ParseState m
start xs =
    ParseState{ past = Past.empty, future = Stream.fromListT xs }

record :: Monad m => Text -> StateT (ParseState m) m ()
record x = modifying pastLens (Past.record x)
