module Stratoparsec.Document.Past where

import Stratoparsec.Buffer.Base (Buffer)
import Stratoparsec.Document.Position (LineNumber, ColumnNumber)

import qualified Map

data Past =
  Past
    { lineMap :: Map LineNumber (Buffer Text)
    , lastCharacterWasCR :: Bool
    }

empty :: Past
empty = Past{ lineMap = Map.empty, lastCharacterWasCR = False }
