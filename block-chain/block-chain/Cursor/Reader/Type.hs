module Cursor.Reader.Type
  (
    Reader, ReaderPlus,
  )
  where

import SupplyChain
import Cursor.Interface
import Block

type Reader action item block product =
    Block item block =>
        Job (Cursor block) action product

type ReaderPlus up action item block product =
    (Block item block, IsCursor block up) =>
        Job up action product
