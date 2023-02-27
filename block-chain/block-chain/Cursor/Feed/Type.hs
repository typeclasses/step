module Cursor.Feed.Type where

import Essentials

import Cursor.Interface (Cursor)
import SupplyChain (Vendor)
import Block (Block)

type Feed action item block =
    Block item block =>
        Vendor (Const Void) (Cursor block) action

type FeedPlus up action item block =
    Block item block =>
        Vendor up (Cursor block) action
