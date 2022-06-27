module Step.Document.Position where

newtype LineNumber = LineNumber Natural
    deriving newtype (Eq, Ord, Num, Show)

newtype ColumnNumber = ColumnNumber Natural
    deriving newtype (Eq, Ord, Num, Show)

data Position = Position{ line :: LineNumber, column :: ColumnNumber }
    deriving stock (Eq, Ord, Show)

makeLensesFor [("line", "lineLens"), ("column", "columnLens")] ''Position

start :: Position
start = Position 1 1
