module Cursor.Feed.Examples.Pushback
  (
    {- * Feed -} pushback,
  )
  where

import Essentials
import Cursor.Feed.Type
import Cursor.Interface.Type

import Pushback.Interface (PushbackStream, push, next)
import SupplyChain (order)

import qualified SupplyChain

{-| Turn a pushback block producer into a cursor feed -}
pushback :: PushbackStream block up => FeedPlus up action item block
pushback = SupplyChain.loop' \case
    Next -> order next
    Push x -> order (push x)
    Flush -> pure ()
