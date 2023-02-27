module Cursor.Feed.Examples
  (
    {- * Feed examples -}
    pushback, substateBuffer, privateBuffer, substate, empty,
  )
  where

import Essentials
import Cursor.Feed.Type
import Cursor.Interface

import Block (Block)
import Control.Monad.State (MonadState)
import Data.Sequence (Seq (..))
import Next (TerminableStream)
import Optics (Lens')
import SupplyChain ((>->))
import Pushback.Interface (PushbackStream)
import SupplyChain (order)

import qualified Pushback.Buffer
import qualified Pushback.Stack
import qualified Pushback.StackContainer as StackContainer
import qualified SupplyChain.Vendor as Vendor
import qualified SupplyChain

{-| Turn a pushback block producer into a cursor feed

When this cursor receives a 'Reset' request, any input that has been read from
upstream but not committed is pushed back up to the source.

See "Cursor.Feed.Examples.Pushback". -}
pushback :: PushbackStream block up => FeedPlus up action item block
pushback = SupplyChain.loop' \case
    Next -> order next
    Push x -> order (push x)
    Flush -> pure ()

{-| No input -}
empty :: FeedPlus up action item block
empty = Vendor.function \case
    Next -> End
    Push _ -> ()
    Flush -> ()

{-| In-memory cursor feed that operates solely on state -}
substate :: forall state up action item block.
    Block item block => MonadState state action =>
    Lens' state (Seq block)
        -- ^ Identifies where to store pushed-back items
        --   within a larger state context
    -> FeedPlus up action item block
substate containerLens = buffer >-> pushback
  where
    buffer = Pushback.Stack.substate containerLens StackContainer.sequence

{-| Turn a block producer into a cursor feed, using state to store remainders

When this cursor receives a 'Reset' request, any input that has been read from
upstream but not committed is pushed into a buffer stored in the 'MonadState'
context, at a position identified by the given lens parameter. -}
substateBuffer :: forall state up action item block.
    Block item block => MonadState state action => TerminableStream block up =>
    Lens' state (Seq block)
        -- ^ Identifies where to store pushed-back items
        --   within a larger state context
    -> FeedPlus up action item block
substateBuffer containerLens = buffer >-> pushback
  where
    buffer = Pushback.Buffer.substate containerLens StackContainer.sequence

{-| Turns a block producer into a cursor feed that does not afford any way
    to retrieve remainders

This kind of feed may be useful where the upstream is 'Next' if you will never
need to use the upstream again. It can also be useful when the upstream is a
`Cursor` that you don't need to commit to, because once you're done using the
private buffer, the upstream cursor can be reset to restore the blocks. -}
privateBuffer :: TerminableStream block up => FeedPlus up action item block
privateBuffer = Pushback.Buffer.private >-> pushback
