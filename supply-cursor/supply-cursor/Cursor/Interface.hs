module Cursor.Interface where

import Cursor.Advancement
import Data.Maybe (Maybe)
import Integer

data Mode = R | RW

data Cursor (mode :: Mode) chunk product =
    ( product ~ Maybe chunk             ) => NextChunk
  | ( product ~ ()                      ) => Reset
  | ( product ~ Advancement, mode ~ 'RW ) => Commit Positive
