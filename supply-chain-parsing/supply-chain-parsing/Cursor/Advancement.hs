module Cursor.Advancement where

import Positive

data Advancement =
    AdvanceSuccess
  | YouCanNotAdvance{ shortfall :: Positive }
