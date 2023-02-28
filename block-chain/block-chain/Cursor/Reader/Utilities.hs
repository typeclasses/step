module Cursor.Reader.Utilities
  (
    {- * Reader utilities -} withBlocks, withLength, lookAhead, firstJust,
            true, just, whileJust, whileRight, whileSuccessful,
  )
  where

import Cursor.Reader.Utilities.Alternative
import Cursor.Reader.Utilities.Exception
import Cursor.Reader.Utilities.LookAhead
import Cursor.Reader.Utilities.With


