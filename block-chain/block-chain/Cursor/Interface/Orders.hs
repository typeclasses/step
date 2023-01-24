module Cursor.Interface.Orders where

import Essentials

import SupplyChain (Job, order)
import Cursor.Interface.Class (IsCursor)
import Next.Interface.Type (Step)
import Cursor.Interface.Type (Advancement (..), Mode (..))
import Integer (Positive, Natural)

import qualified Cursor.Interface.Class as Cursor
import qualified Integer.Positive as Positive

next :: IsCursor mode block up => Job up action (Step block)
next = order Cursor.next

reset :: IsCursor mode block up => Job up action ()
reset = order Cursor.reset

commitPositive :: IsCursor 'Write block up => Positive -> Job up action Advancement
commitPositive n = order (Cursor.commit n)

commitNatural :: IsCursor 'Write block up => Natural -> Job up action Advancement
commitNatural n = case Positive.fromNatural n of
    Nothing -> pure AdvanceSuccess
    Just p -> commitPositive p
