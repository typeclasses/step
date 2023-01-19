module Cursor.ResetReader.Utilities.With
  (
    withBlocks, withLength,
  )
  where

import Essentials
import Cursor.ResetReader.Type
import Cursor.Interface.Type

import Cursor.ResetReader.Examples.Take (takeNatural)
import Data.Sequence (Seq)
import Integer (Natural)
import SupplyChain ((>-))
import Prelude ((+), (-))

import qualified Cursor.Feed.Examples as Feed
import qualified Monitor
import qualified Integer.Positive as Positive
import qualified SupplyChain

{-| Augments a reader's result with the exact input that was committed over -}
withBlocks :: ResetReader action 'Write block product
    -> ResetReaderPlus up action 'Write block (Seq block, product)
withBlocks (ResetReader x) = do
    (length, product) <- ResetReader $
        Feed.privateBuffer >- Monitor.withRecord lengthRecording x
    (_, blocks) <- takeNatural length
    pure (blocks, product)

{-| Augments a reader's result with the amount of input that is committed -}
withLength :: ResetReader action 'Write block product
    -> ResetReaderPlus up action 'Write block (Natural, product)
withLength (ResetReader x) = do
    (length, product) <- ResetReader $
        Feed.privateBuffer >- Monitor.withRecord lengthRecording x
    _ <- takeNatural length
    pure (length, product)

lengthRecording ::
    Monitor.Recording (Cursor 'Write block) (SupplyChain.Unit Natural) Natural action
lengthRecording = Monitor.Recording
    { Monitor.initial = 0
    , Monitor.step = \case
          Reset -> \_ -> pure
          Next -> \_ -> pure
          Commit n -> \advancement runningTotal -> pure $
              runningTotal & (+) (Positive.toNatural n) & minusShortfall advancement
    , Monitor.extract = \SupplyChain.Unit -> pure
    }

minusShortfall :: Advancement -> Natural -> Natural
minusShortfall = \case
    AdvanceSuccess -> id
    YouCanNotAdvance{ shortfall } -> \x -> x - Positive.toNatural shortfall
