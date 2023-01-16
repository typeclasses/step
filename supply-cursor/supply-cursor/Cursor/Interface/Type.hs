module Cursor.Interface.Type where

import Integer (Positive)
import Next.Interface (Next (..), Step (..), TerminableStream (..))

data Mode = R | RW

data Cursor (mode :: Mode) block product =
    ( product ~ Step block              ) => NextBlock
  | ( product ~ ()                      ) => Reset
  | ( product ~ Advancement, mode ~ 'RW ) => Commit Positive

data Advancement =
    AdvanceSuccess
  | YouCanNotAdvance{ shortfall :: Positive }

instance TerminableStream block (Cursor mode block) where
    liftNext Next = NextBlock
