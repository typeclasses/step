module Cursor.Morph.Usage where

import Essentials
import Cursor.Reader.Type
import Cursor.Morph.Type

import SupplyChain ((>-))
import Block.Class.Class (Block)

import qualified Cursor.Interface.Orders as Order

morphed ::
    Block block2 => -- todo: why is this constraint needed?
    MorphPlus up action mode block1 block2
    -> Reader action mode block2 product
    -> ReaderPlus up action mode block1 product
morphed t (Reader r) =
    Reader $ t >- (r <* Order.flush)
