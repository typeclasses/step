module Cursor.Reader.Utilities.LookAhead
  (
    lookAhead,
  )
  where

import Cursor.Reader.Type

import Cursor.Interface (Mode (..))
import SupplyChain ((>-))

import qualified Cursor.Feed.Examples as Feed

lookAhead :: Reader action 'Write item block product
    -> ReaderPlus up action mode item block product
lookAhead (Reader x) = Reader (Feed.privateBuffer >- x)
