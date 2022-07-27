module Step.Cursor
  (
    {- * Cursor -} Cursor (..), AdvanceResult (..), rebaseCursor, recurseCursor,
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

rebaseCursor :: (forall a. base1 a -> base2 a) -> Cursor xs x base1 cursor -> Cursor xs x base2 cursor
rebaseCursor = Cursor.rebase

recurseCursor :: (forall a. Iso' (cursor1 a) (cursor2 a)) -> Cursor xs x base cursor1 -> Cursor xs x base cursor2
recurseCursor = Cursor.recurse

rebaseStream :: (forall a. m1 a -> m2 a) -> Stream m1 xs x -> Stream m2 xs x
rebaseStream = Stream.rebase

streamChoice :: Monad m => Stream m xs x -> Stream m xs x -> Stream m xs x
streamChoice = Stream.choice
