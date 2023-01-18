module Cursor.Reader.Type where

import Block.Class (Block)
import Cursor.Interface (Cursor, IsCursor)
import SupplyChain (Job)

type Reader action mode block product =
    Block block =>
        Job (Cursor mode block) action product

type ReaderPlus up action mode block product =
    Block block => IsCursor mode block up =>
        Job up action product
