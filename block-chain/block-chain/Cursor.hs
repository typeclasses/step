module Cursor
  (
    {- * Types -} Reader, ReaderPlus (..), Feed, FeedPlus, Morph, MorphPlus, Advancement (..), Shortfall (..),
    {- * Reader -} takePositive,
    {- * Morph -} morph, decodeAscii,
    {- * Running readers -} readBlockList,
  )
  where

import Cursor.Advancement
import Cursor.Feed
import Cursor.Morph
import Cursor.Reader

import Block (Shortfall (..))
