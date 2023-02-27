module Cursor.Feed.Type where

import Essentials

import Cursor.Interface (Cursor, Mode)
import SupplyChain (Vendor)
import Block (Block)

type Feed action (mode :: Mode) item block =
    Block item block =>
        Vendor (Const Void) (Cursor mode block) action

type FeedPlus up action (mode :: Mode) item block =
    Block item block =>
        Vendor up (Cursor mode block) action
