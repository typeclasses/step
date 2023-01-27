module Cursor.Transcoder.Usage where

import Essentials
import Cursor.Reader.Type
import Cursor.Transcoder.Type

import SupplyChain ((>-))
import Block.Class (Block)

import qualified Cursor.Interface.Orders as Order

transcoded ::
    Block block2 => -- todo: why is this constraint needed?
    TranscoderPlus up action mode block1 block2
    -> Reader action mode block2 product
    -> ReaderPlus up action mode block1 product
transcoded t (Reader r) =
    Reader $ t >- (r <* Order.flush)
