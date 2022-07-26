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
import Step.Cursor.ChunkStream (Stream, stream, next, record, list)
import Step.Cursor.Class (Cursory (..))
import Step.Cursor.StreamCompletion (StreamCompletion (..))
import Step.Cursor.Type (Cursor (..))
import Step.Cursor.InputChunking (genChunks)

import qualified Step.Cursor.Type as Cursor
import qualified Step.Cursor.ChunkStream as Stream

rebaseCursor :: (forall a. m1 a -> m2 a) -> Cursor xs x m1 -> Cursor xs x m2
rebaseCursor = Cursor.rebase

rebaseStream :: (forall a. m1 a -> m2 a) -> Stream m1 xs x -> Stream m2 xs x
rebaseStream = Stream.rebase

streamChoice :: Monad m => Stream m xs x -> Stream m xs x -> Stream m xs x
streamChoice = Stream.choice
