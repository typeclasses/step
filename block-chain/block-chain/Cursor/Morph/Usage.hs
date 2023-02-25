module Cursor.Morph.Usage where

import Essentials
import Cursor.Reader.Type
import Cursor.Morph.Type

import SupplyChain ((>-))
import Block (Block)

import qualified Cursor.Interface.Orders as Order

morphed ::
    Block item2 block2 => -- todo: why is this constraint needed?
    MorphPlus up action mode item1 block1 item2 block2
    -> Reader action mode item2 block2 product
    -> ReaderPlus up action mode item1 block1 product
morphed t (Reader r) =
    Reader $ t >- (r <* Order.flush)
