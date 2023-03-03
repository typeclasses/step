module Cursor.Reader
  (
    {- * Types -} Reader, ReaderPlus (..),
    {- * Examples -}
            takePositive, takeNatural,
            atEnd, remainsAtLeastPositive, remainsAtLeastNatural,
    {- ** Exact -} exact, enum, enumErrorText, enumExceptText,
    {- * Utilities -}
            withBlocks, withLength, lookAhead, firstJust,
            true, just, whileJust, whileRight, whileSuccessful,
            whileSuccessfulJust, whileJustExcept,
    {- * Usage -} readBlockList,
  )
  where

import Cursor.Reader.Type
import Cursor.Reader.Utilities
import Cursor.Reader.Examples
import Cursor.Reader.Usage
