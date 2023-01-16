module Cursor.Interface.Type where

import Integer (Positive)
import Next.Interface (Next (..), Step (..), TerminableStream (..))

data Mode = Read | Write

data Cursor (mode :: Mode) block product =
    ( product ~ Step block                 ) => NextBlock
  | ( product ~ ()                         ) => Reset
  | ( product ~ Advancement, mode ~ 'Write ) => Commit Positive

type CursorRead block product =
    Cursor 'Read block product

type CursorWrite block product =
    forall mode. Cursor mode block product

data Advancement =
    AdvanceSuccess
  | YouCanNotAdvance{ shortfall :: Positive }

instance TerminableStream block (Cursor mode block) where
    liftNext Next = NextBlock
