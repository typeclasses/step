module Cursor.Morph.Type
  (
    {- * Types -} Morph, MorphPlus,
  )
  where

import Block (Block)
import Cursor.Interface (Cursor, Mode, IsCursor)
import SupplyChain (Vendor)

type Morph action (mode :: Mode) item1 block1 item2 block2 =
    Block item1 block1 => Block item2 block2 =>
        Vendor (Cursor mode block1) (Cursor mode block2) action

type MorphPlus up action (mode :: Mode) item1 block1 item2 block2 =
    Block item1 block1 => Block item2 block2 => IsCursor mode block1 up =>
        Vendor up (Cursor mode block2) action
