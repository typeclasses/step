module Cursor.Interface.Class where

import Cursor.Interface.Type

class IsCursor (mode :: Mode) block interface where
    liftCursor :: Cursor mode block product -> interface product

instance IsCursor mode block (Cursor mode block) where
    liftCursor x = x
