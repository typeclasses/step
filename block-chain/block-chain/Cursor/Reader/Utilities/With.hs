module Cursor.Reader.Utilities.With
  (
    withBlocks, withLength,
  )
  where

import Essentials
import Cursor.Reader.Type
import Cursor.Interface.Type

import Cursor.Advancement (minusShortfall)
import Cursor.Reader.Examples.TakeNumber (takeNatural)
import Data.Sequence (Seq)
import Integer (Natural)
import SupplyChain ((>-))
import Prelude ((+))

import qualified Cursor.Feed.Examples as Feed
import qualified Monitor
import qualified Integer.Positive as Positive
import qualified SupplyChain

{-| Augments a reader's result with the exact input that was committed over -}
withBlocks :: Reader action 'Write item block product
    -> ReaderPlus up action 'Write item block (Seq block, product)
withBlocks (Reader x) = do
    (length, product) <- Reader $
        Feed.privateBuffer >- Monitor.withRecord lengthRecording x
    (_, blocks) <- takeNatural length
    pure (blocks, product)

{-| Augments a reader's result with the amount of input that is committed -}
withLength :: Reader action 'Write item block product
    -> ReaderPlus up action 'Write item block (Natural, product)
withLength (Reader x) = do
    (length, product) <- Reader $
        Feed.privateBuffer >- Monitor.withRecord lengthRecording x
    _ <- takeNatural length
    pure (length, product)

lengthRecording ::
    Monitor.Recording (Cursor 'Write block) (SupplyChain.Unit Natural) Natural action
lengthRecording = Monitor.Recording
    { Monitor.initial = 0
    , Monitor.step = \case
          Reset -> \_ -> pure
          Flush -> \_ -> pure
          Next -> \_ -> pure
          Commit n -> \advancement runningTotal -> pure $
              runningTotal & (+) (Positive.toNatural n) & minusShortfall advancement
    , Monitor.extract = \SupplyChain.Unit -> pure
    }
