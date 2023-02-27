module Cursor.Interface.Orders where

import SupplyChain (Job, order)
import Cursor.Interface.Class (IsCursor)
import Next.Interface.Type (Step)

import qualified Cursor.Interface.Class as Cursor

next :: IsCursor block up => Job up action (Step block)
next = order Cursor.next

flush :: IsCursor block up => Job up action ()
flush = order Cursor.flush

push :: IsCursor block up => block -> Job up action ()
push x = order (Cursor.push x)
