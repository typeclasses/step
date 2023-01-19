module Cursor.ResetReader.Utilities.With
  (
    withBlocks, withLength,
  )
  where

import Essentials
import Cursor.ResetReader.Type

import Cursor.Counting.Examples (counting)
import Cursor.Counting.Type (Counting (Order, AmountCommitted))
import Cursor.Interface (Mode (..))
import Cursor.ResetReader.Examples.Take (takeNatural)
import Data.Sequence (Seq)
import Integer (Natural)
import SupplyChain ((>->), (>-), order)

import qualified Cursor.Feed.Examples as Feed
import qualified SupplyChain.Vendor as Vendor

{-| Augments a reader's result with the exact input that was committed over -}
withBlocks :: ResetReader action 'Write block product
    -> ResetReaderPlus up action 'Write block (Seq block, product)
withBlocks x = do
    (length, product) <- withLength x
    (_, blocks) <- takeNatural length
    pure (blocks, product)

{-| Augments a reader's result with the amount of input that is committed -}
withLength :: ResetReader action 'Write block product
    -> ResetReaderPlus up action 'Write block (Natural, product)
withLength (ResetReader x) =
    ResetReader (Feed.privateBuffer >-> counting >- y)
  where
    y = do
        product <- Vendor.map Order >- x
        length <- SupplyChain.order AmountCommitted
        pure (length, product)
