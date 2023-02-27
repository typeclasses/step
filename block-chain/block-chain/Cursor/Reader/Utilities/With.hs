module Cursor.Reader.Utilities.With
  (
    withBlocks, withLength,
  )
  where

import Essentials
import Cursor.Reader.Type
import Cursor.Interface.Type

import Cursor.Advancement (minusShortfall)
import Cursor.Reader.Examples.Take (takeNatural)
import Data.Sequence (Seq)
import Integer (Natural)
import SupplyChain ((>-))
import Prelude ((+))

import qualified Cursor.Feed.Examples as Feed
import qualified Monitor
import qualified Integer.Positive as Positive
import qualified SupplyChain

{-| Augments a reader's result with the exact input that was committed over -}
withBlocks :: Reader action item block product
    -> ReaderPlus up action item block (Seq block, product)
withBlocks x = do
    (length, product) <- Reader $
        Feed.privateBuffer >- Monitor.withRecord lengthRecording x
    (_, blocks) <- takeNatural length
    pure (blocks, product)

{-| Augments a reader's result with the amount of input that is committed -}
withLength :: Reader action item block product
    -> ReaderPlus up action item block (Natural, product)
withLength x = do
    (length, product) <- Reader $
        Feed.privateBuffer >- Monitor.withRecord lengthRecording x
    _ <- takeNatural length
    pure (length, product)

lengthRecording ::
    Monitor.Recording (Cursor block) (SupplyChain.Unit Natural) Natural action
lengthRecording = Monitor.Recording
    { Monitor.initial = 0
    , Monitor.step = \cases
          Flush     () total -> pure $ total
          Next      xs total -> pure $ total + Block.length xs
          (Push xs) () total -> pure $ total - Block.length xs
    , Monitor.extract = \SupplyChain.Unit -> pure
    }
