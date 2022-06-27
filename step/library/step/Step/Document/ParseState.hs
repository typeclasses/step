module Step.Document.ParseState where

import Step.Internal.Prelude

import Step.Document.Position (Position)

import Step.Stream.Base (Stream)
import qualified Step.Stream.Base as Stream

import Step.Document.Past (Past)
import qualified Step.Document.Past as Past

data ParseState text m =
  ParseState
    { past :: Past text
    , future :: Stream m text
    }

makeLensesFor [("future", "futureLens"), ("past", "pastLens")] ''ParseState

positionLens :: Lens' (ParseState text m) Position
positionLens = pastLens % Past.positionLens

start :: ListT m text -> ParseState text m
start xs =
    ParseState{ past = Past.empty, future = Stream.fromListT xs }

record :: Monad m => ListLike text Char => text -> StateT (ParseState text m) m ()
record x = modifying pastLens (Past.record x)
