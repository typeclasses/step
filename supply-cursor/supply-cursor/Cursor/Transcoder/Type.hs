module Cursor.Transcoder.Type where

import Cursor.Interface
import SupplyChain

type Transcoder action (mode :: Mode) block =
    Vendor (Cursor mode block) (Cursor mode block) action

type TranscoderPlus up action (mode :: Mode) block =
    IsCursor mode block up =>
        Vendor up (Cursor mode block) action
