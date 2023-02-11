module Cursor.Feed.Type where

import Essentials

import Cursor.Interface (Cursor, Mode)
import SupplyChain (Vendor)
import Block.Class.Class (Block)

type Feed action (mode :: Mode) block =
    Block block =>
        Vendor (Const Void) (Cursor mode block) action

type FeedPlus up action (mode :: Mode) block =
    Block block =>
        Vendor up (Cursor mode block) action
