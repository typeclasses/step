module Cursor.Feed.Examples
  (
    {- * Feed examples -}
    pushback, substateBuffer, privateBuffer, substate, empty,
  )
  where

import Essentials
import Cursor.Feed.Type
import Cursor.Interface


import Block.Class (Block)
import Control.Monad.State (MonadState)
import Data.Sequence (Seq (..))
import Next (TerminableStream)
import Optics (Lens')
import SupplyChain (Vendor (Vendor), Referral (Referral), (>->))
import Pushback.Interface (PushbackStream)

import qualified Cursor.Feed.Examples.Pushback as Pushback
import qualified Pushback.Buffer
import qualified Pushback.Stack
import qualified Pushback.StackContainer as StackContainer

{-| Turn a pushback block producer into a cursor feed

When this cursor receives a 'Reset' request, any input that has been read from
upstream but not committed is pushed back up to the source.

See "Cursor.Feed.Examples.Pushback". -}
pushback :: forall up block action mode. PushbackStream block up =>
    FeedPlus up action mode block
pushback = Pushback.pushback

{-| No input

'Next' always returns 'End', 'Commit' always returns 'YouCanNotAdvance',
and 'Reset' does nothing. -}
empty :: FeedPlus up action mode block
empty = Vendor \case
    Reset -> pure $ Referral () empty
    Next -> pure $ Referral End empty
    Commit n -> pure $ Referral YouCanNotAdvance{ shortfall = n } empty

{-| In-memory cursor feed that operates solely on state -}
substate :: forall state up action block mode.
    Block block => MonadState state action =>
    Lens' state (Seq block)
        -- ^ Identifies where to store pushed-back items
        --   within a larger state context
    -> FeedPlus up action mode block
substate containerLens = buffer >-> pushback
  where
    buffer = Pushback.Stack.substate containerLens StackContainer.sequence

{-| Turn a block producer into a cursor feed, using state to store remainders

When this cursor receives a 'Reset' request, any input that has been read from
upstream but not committed is pushed into a buffer stored in the 'MonadState'
context, at a position identified by the given lens parameter. -}
substateBuffer :: forall state up action block mode.
    Block block => MonadState state action => TerminableStream block up =>
    Lens' state (Seq block)
        -- ^ Identifies where to store pushed-back items
        --   within a larger state context
    -> FeedPlus up action mode block
substateBuffer containerLens = buffer >-> pushback
  where
    buffer = Pushback.Buffer.substate containerLens StackContainer.sequence

{-| Turns a block producer into a cursor feed that does not afford any way
    to retrieve remainders

This kind of feed may be useful where the upstream is 'Next' if you will never
need to use the upstream again. It can also be useful when the upstream is a
`Cursor` that you don't need to commit to, because once you're done using the
private buffer, the upstream cursor can be reset to restore the blocks. -}
privateBuffer :: TerminableStream block up => FeedPlus up action mode block
privateBuffer = Pushback.Buffer.private >-> pushback
