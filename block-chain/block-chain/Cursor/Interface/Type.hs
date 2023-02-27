module Cursor.Interface.Type
  (
    {- * The interface -} Cursor (..),
    {- * Supporting types -} Advancement (..), Step (..),
  )
  where

import Cursor.Advancement.Type (Advancement (..))
import Pushback (PushbackStream, Step (..), TerminableStream)

import qualified Next
import qualified Pushback.Interface as Pushback

data Cursor block product =
    ( product ~ Step block  ) => Next
  | ( product ~ ()          ) => Push block
  | ( product ~ ()          ) => Flush

instance TerminableStream block (Cursor block) where
    liftNext Next.Next = Next

instance PushbackStream block (Cursor block) where
    liftPushback = \case
        Pushback.Next -> Next
        Pushback.Push x -> Push x
