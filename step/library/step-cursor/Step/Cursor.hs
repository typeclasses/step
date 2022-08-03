{-# language PatternSynonyms #-}

module Step.Cursor
  (
    {- * Cursor -} ReadWriteCursor (..), AdvanceResult (..), pattern Run, inp, com, runn,
        rebaseCursor, lookAhead_,
    {- * Class -} Cursory (..),
    {- * Stream -}
        Stream (..), stream, streamRST, StreamCompletion (..),
        record,
        list, streamChoice,
    {- * Testing -} genChunks,
  )
  where

import Step.Cursor.AdvanceResult
import Step.Cursor.Stream
import Step.Cursor.Class
import Step.Cursor.StreamCompletion
import Step.Cursor.ReadWriteCursor
import Step.Cursor.InputChunking
