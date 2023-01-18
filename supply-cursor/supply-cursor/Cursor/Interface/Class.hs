module Cursor.Interface.Class
  (
    {- * Class -} IsCursor (..),
    {- * Requests -} commit, reset, next,
  )
  where

import Cursor.Interface.Type

import Integer (Positive)
import Next (TerminableStream, next)

class TerminableStream block interface =>
    IsCursor (mode :: Mode) block interface
    | interface -> mode block
  where
    liftCursor :: Cursor mode block product -> interface product

instance IsCursor mode block (Cursor mode block) where
    liftCursor x = x

commit :: IsCursor 'Write block up => Positive -> up Advancement
commit x = liftCursor (Commit x)

reset :: IsCursor mode block up => up ()
reset = liftCursor Reset
