module Cursor.Interface.Type
  (
    {- * The interface -} Cursor (..),
    {- * Supporting types -} Mode (..), Advancement (..), Step (..),
    {- * Aliases -} CursorRead, CursorWrite,
  )
  where

import Cursor.Advancement.Type (Advancement (..))
import Integer (Positive)
import Next.Interface (Step (..), TerminableStream (..))

import qualified Next

data Mode = Read | Write

data Cursor (mode :: Mode) block product =
    ( product ~ Step block                 ) => Next
  | ( product ~ ()                         ) => Reset
  | ( product ~ Advancement, mode ~ 'Write ) => Commit Positive

type CursorRead block product =
    Cursor 'Read block product

type CursorWrite block product =
    forall mode. Cursor mode block product

instance TerminableStream block (Cursor mode block) where
    liftNext Next.Next = Next
