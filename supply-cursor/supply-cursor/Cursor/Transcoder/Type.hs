module Cursor.Transcoder.Type where

import Block.Class (Block)
import Cursor.Interface (Cursor, Mode, IsCursor)
import SupplyChain (Vendor)

type Transcoder action (mode :: Mode) block =
    Block block =>
        Vendor (Cursor mode block) (Cursor mode block) action

type TranscoderPlus up action (mode :: Mode) block =
    Block block => IsCursor mode block up =>
        Vendor up (Cursor mode block) action
