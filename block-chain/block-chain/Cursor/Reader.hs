module Cursor.Reader
  (
    {- * Types -} Reader, ReaderPlus (..),
    {- * Examples -}
            takePositive, takeNatural,
            atEnd, remainsAtLeastPositive, remainsAtLeastNatural,
    {- ** Exact -} exact, enum,
    {- * Utilities -}
            withBlocks, withLength, lookAhead, firstJust,
            true, just, whileJust, whileRight, whileSuccessful,
            whileSuccessfulJust, whileJustExcept,
  )
  where

import Cursor.Reader.Type
import Cursor.Reader.Utilities
import Cursor.Reader.Examples
