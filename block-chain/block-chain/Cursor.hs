module Cursor
  (
    {- * Types -} Reader, ReaderPlus (..), Feed, FeedPlus, Morph, MorphPlus, Advancement (..), Shortfall (..),
    {- * Reader -} takePositive, enum, enumErrorText, enumExceptText, takeWhile, span,
    {- * Morph -} morph, decodeAscii,
    {- * Running readers -} readBlockList,
    {- * Miscellany -} overExcept,
  )
  where

import Cursor.Advancement
import Cursor.Feed
import Cursor.Morph
import Cursor.Reader

import Miscellany

import Block (Shortfall (..))
