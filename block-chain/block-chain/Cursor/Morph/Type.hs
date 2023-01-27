module Cursor.Morph.Type
  (
    {- * Types -} Morph, MorphPlus,
  )
  where

import Block.Class (Block)
import Cursor.Interface (Cursor, Mode, IsCursor)
import SupplyChain (Vendor)

type Morph action (mode :: Mode) block1 block2 =
    Block block1 => Block block2 =>
        Vendor (Cursor mode block1) (Cursor mode block2) action

type MorphPlus up action (mode :: Mode) block1 block2 =
    Block block1 => Block block2 => IsCursor mode block1 up =>
        Vendor up (Cursor mode block2) action
