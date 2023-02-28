module Cursor.Morph.Usage where

import Essentials
import Cursor.Reader.Type
import Cursor.Morph.Type

import SupplyChain ((>-))
import Block (Block)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)

import qualified Cursor.Interface.Orders as Order

morph ::
    Block item2 block2 => -- todo: why is this constraint needed?
    MorphPlus up action mode item1 block1 item2 block2
    -> Reader action mode item2 block2 product
    -> ReaderPlus up action mode item1 block1 product
morph t (Reader r) =
    Reader $ t >- (r <* Order.flush)

morphExcept ::
    Block item2 block2 => -- todo: why is this constraint needed?
    MorphPlus up action mode item1 block1 item2 block2
    -> ExceptT error (Reader action mode item2 block2) product
    -> ExceptT error (ReaderPlus up action mode item1 block1) product
morphExcept t = runExceptT >>> morph t >>> ExceptT
