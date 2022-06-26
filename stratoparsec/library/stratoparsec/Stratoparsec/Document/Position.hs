module Stratoparsec.Document.Position where

newtype LineNumber = LineNumber Natural
    deriving newtype (Eq, Ord, Num, Show)

newtype ColumnNumber = ColumnNumber Natural
    deriving newtype (Eq, Ord, Num, Show)

data Position = Position{ line :: LineNumber, column :: ColumnNumber }

start :: Position
start = Position 0 0
