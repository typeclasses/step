module Step.Document.ParseState where

import Optics

import ListT (ListT)

import Step.Document.Position (Position)

import Step.Stream.Base (Stream)
import qualified Step.Stream.Base as Stream

import Step.Document.Past (Past)
import qualified Step.Document.Past as Past

data ParseState m =
  ParseState
    { past :: Past
    , future :: Stream m Text
    }

makeLensesFor [("future", "futureLens"), ("past", "pastLens")] ''ParseState

positionLens :: Lens' (ParseState m) Position
positionLens = pastLens % Past.positionLens

start :: ListT m Text -> ParseState m
start xs =
    ParseState{ past = Past.empty, future = Stream.fromListT xs }

record :: Monad m => Text -> StateT (ParseState m) m ()
record x = modifying pastLens (Past.record x)
