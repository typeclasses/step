module Step.Cursor
  (
    {- * Cursor -} Cursor (..), AdvanceResult (..), rebaseCursor,
    {- * Class -} Cursory (..),
    {- * Stream -} Stream, stream, next, StreamCompletion (..), rebaseStream, record, list, streamChoice,
    {- * Testing -} genChunks,
  )
  where

import Step.Internal.Prelude

import Step.Cursor.AdvanceResult (AdvanceResult (..))
import Step.Cursor.Stream (Stream, stream, next, record, list, rebaseStream, streamChoice)
import Step.Cursor.Class (Cursory (..))
import Step.Cursor.StreamCompletion (StreamCompletion (..))
import Step.Cursor.Type (Cursor (..), rebaseCursor)
import Step.Cursor.InputChunking (genChunks)
