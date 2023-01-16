module Cursor.Feed.Type where

import Cursor.Interface
import Essentials
import SupplyChain

type Feed action (mode :: Mode) block =
    Vendor (Const Void) (Cursor mode block) action

type FeedPlus up action (mode :: Mode) block =
    Vendor up (Cursor mode block) action
