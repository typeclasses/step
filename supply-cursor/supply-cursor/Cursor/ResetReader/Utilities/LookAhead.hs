module Cursor.ResetReader.Utilities.LookAhead
  (
    lookAhead,
  )
  where

import Cursor.ResetReader.Type

import Cursor.Interface (Mode (..))
import SupplyChain ((>-))

import qualified Cursor.Feed.Examples as Feed

lookAhead :: ResetReader action 'Write block product
    -> ResetReaderPlus up action mode block product
lookAhead (ResetReader x) = ResetReader (Feed.privateBuffer >- x)
