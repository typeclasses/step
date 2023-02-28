module Cursor.Reader.Examples
  (
    {- * Fixed length -} takePositive, takeNatural,
    {- * Amount remaining -} atEnd, remainsAtLeastPositive, remainsAtLeastNatural,
    {- * Exact -} exact, enum,
  )
  where

import Cursor.Reader.Examples.AmountRemaining
import Cursor.Reader.Examples.Exact
import Cursor.Reader.Examples.Take
