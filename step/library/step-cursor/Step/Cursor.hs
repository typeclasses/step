module Step.Cursor
  (
    {- * Cursor -} ReadWriteCursor (..), AdvanceResult (..),
        rebaseCursor,
        -- expandStateCursor, expandStateCursor',
        contramapCursor,
        -- changeStateCursor,
    {- * Class -} Cursory (..),
    {- * Stream -}
        Stream (..), stream, streamRST, StreamCompletion (..),
        record,
        list, streamChoice,
    {- * Testing -} genChunks,
  )
  where

import Step.Internal.Prelude

import Step.Cursor.AdvanceResult
import Step.Cursor.Stream
import Step.Cursor.Class
import Step.Cursor.StreamCompletion
import Step.Cursor.Type
import Step.Cursor.InputChunking
