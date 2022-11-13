module Cursor.Interface where

import Cursor.Advancement
import Data.Maybe (Maybe)
import Positive

data Mode = R | RW

data Cursor (mode :: Mode) item product =
    (product ~ Maybe item) => NextMaybe
  | (product ~ ()) => Reset
  | (product ~ Advancement, mode ~ 'RW) => Commit Positive
