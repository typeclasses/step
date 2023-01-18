module Cursor.Reader.Type where

import Cursor.Interface
import SupplyChain

type Reader action mode block product =
    Job (Cursor mode block) action product

type ReaderPlus up action mode block product =
    IsCursor mode block up =>
        Job up action product
