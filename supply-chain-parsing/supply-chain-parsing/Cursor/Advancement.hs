module Cursor.Advancement where

import Integer

data Advancement =
    AdvanceSuccess
  | YouCanNotAdvance{ shortfall :: Positive }
