module Cursor.Interface.Type where

import Data.Maybe (Maybe)
import Integer (Positive)

data Mode = R | RW

data Cursor (mode :: Mode) chunk product =
    ( product ~ Maybe chunk             ) => NextChunk
  | ( product ~ ()                      ) => Reset
  | ( product ~ Advancement, mode ~ 'RW ) => Commit Positive

data Advancement =
    AdvanceSuccess
  | YouCanNotAdvance{ shortfall :: Positive }
