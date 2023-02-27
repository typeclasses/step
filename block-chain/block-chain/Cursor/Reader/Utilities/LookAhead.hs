module Cursor.Reader.Utilities.LookAhead
  (
    lookAhead,
  )
  where

import Cursor.Reader.Type

import SupplyChain ((>-))

import qualified Cursor.Feed.Examples as Feed

lookAhead :: Reader action item block product
    -> ReaderPlus up action item block product
lookAhead x = Feed.privateBuffer >- x
