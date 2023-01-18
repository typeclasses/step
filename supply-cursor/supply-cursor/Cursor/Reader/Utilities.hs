module Cursor.Reader.Utilities
  (
    {- * Reader utilities -}
    withBlocks, withLength,
  )
  where

import Essentials

import Cursor.Counting.Examples (counting)
import Cursor.Counting.Type (Counting (Order, AmountCommitted))
import Cursor.Interface.Type (Mode (..))
import Cursor.Reader.Type (ReaderPlus, Reader)
import Cursor.Reader.Examples (takeNatural)
import Data.Sequence (Seq)
import Integer (Natural)
import SupplyChain ((>->), (>-))

import qualified Cursor.Feed.Examples as Feed
import qualified SupplyChain
import qualified SupplyChain.Vendor as Vendor

withBlocks :: Reader action 'Write block product
    -> ReaderPlus up action 'Write block (Seq block, product)
withBlocks x = do
    (length, product) <- withLength x
    (_, blocks) <- takeNatural length
    pure (blocks, product)

withLength :: Reader action 'Write block product
    -> ReaderPlus up action 'Write block (Natural, product)
withLength x = Feed.privateBuffer >-> counting >- do
    product <- Vendor.map Order >- x
    length <- SupplyChain.order AmountCommitted
    pure (length, product)
