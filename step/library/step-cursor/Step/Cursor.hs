{-# language PatternSynonyms #-}

module Step.Cursor
  (
    module Step.Cursor.Cursor,
    AdvanceResult (..),
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
import Step.Cursor.Cursor
import Step.Cursor.InputChunking
