module Cursor.Interface.Class
  (
    {- * Class -} IsCursor (..),
    {- * Requests -} next, push, flush,
  )
  where

import Cursor.Interface.Type

import Pushback (PushbackStream, next, push)

class (PushbackStream block interface) =>
    IsCursor block interface | interface -> block
  where
    liftCursor :: Cursor block product -> interface product

instance IsCursor block (Cursor block) where
    liftCursor x = x

flush :: IsCursor block up => up ()
flush = liftCursor Flush
