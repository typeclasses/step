module Stratoparsec.Document.Past where

import Stratoparsec.Buffer.Base (Buffer)
import Stratoparsec.Document.Position (LineNumber, ColumnNumber, Position)

import qualified Stratoparsec.Document.Position as Position

import qualified Map

data Past =
  Past
    { lineMap :: Map LineNumber (Buffer Text)
    , lastCharacterWasCR :: Bool
    , position :: Position
    }

empty :: Past
empty =
  Past
    { lineMap = Map.empty
    , lastCharacterWasCR = False
    , position = Position.start
    }

record :: Text -> Past -> Past
record = _
