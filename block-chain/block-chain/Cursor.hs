module Cursor
  (
    {- * Types -} Reader, ReaderPlus (..), Feed, FeedPlus, Morph, MorphPlus, Advancement (..),
    {- * Reader -} takePositive,
    {- * Running readers -} readBlockList,
  )
  where

import Cursor.Feed
import Cursor.Morph
import Cursor.Reader
import Cursor.Advancement
