{-# language PatternSynonyms #-}

module Step.Cursor
  (
    {- * Cursor -} CursorRW, AdvanceResult (..), RunCursorRW, runCursorRW,
        rebaseCursor,
    {- * Stream -}
        Stream (..), stream, streamRST, StreamCompletion (..),
        record,
        list, streamChoice,
    {- * Testing -} genChunks,
  )
  where

import Step.Cursor.AdvanceResult
import Step.Cursor.Stream
import Step.Cursor.StreamCompletion
import Step.Cursor.CursorRW
import Step.Cursor.InputChunking
import Step.Cursor.RunCursorRW
