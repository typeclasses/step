module Block.End where

data End = Front | Back

opposite :: End -> End
opposite = \case Front -> Back; Back -> Front
