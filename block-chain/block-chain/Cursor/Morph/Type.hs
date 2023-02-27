module Cursor.Morph.Type
  (
    {- * Types -} Morph, MorphPlus,
  )
  where

import Block (Block)
import Cursor.Interface (Cursor, IsCursor)
import SupplyChain (Vendor)

type Morph action item1 block1 item2 block2 =
    Block item1 block1 => Block item2 block2 =>
        Vendor (Cursor block1) (Cursor block2) action

type MorphPlus up action item1 block1 item2 block2 =
    Block item1 block1 => Block item2 block2 => IsCursor block1 up =>
        Vendor up (Cursor block2) action
