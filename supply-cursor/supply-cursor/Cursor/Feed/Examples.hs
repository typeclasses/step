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
import Block.Class (Drop (..), drop)
import Control.Monad.State (MonadState)
import Control.Monad.State (StateT)
import Control.Monad.Trans (lift)
import Data.Sequence (Seq (..))
import Integer (Positive)
import Next (TerminableStream)
import Optics (Lens', assign, use, modifying, view)
import Pushback.Interface (PushbackStream, push)
import SupplyChain (Job, Vendor (Vendor), Referral (Referral), (>->))

import qualified Control.Monad.State as State
import qualified Data.Sequence as Seq
import qualified Optics
import qualified Pushback.Buffer
import qualified Pushback.Stack
import qualified Pushback.StackContainer as StackContainer
import qualified SupplyChain.Job as Job

{-| No input

'Next' always returns 'End', 'Commit' always returns 'YouCanNotAdvance',
and 'Reset' does nothing. -}
empty :: FeedPlus up action mode block
empty = Vendor \case
    Reset -> pure $ Referral () empty
    Next -> pure $ Referral End empty
    Commit n -> pure $ Referral YouCanNotAdvance{ shortfall = n } empty

data DoubleBuffer c = DoubleBuffer (Seq c) (Seq c)

uncommitted, unviewed :: Optics.Lens (DoubleBuffer c) (DoubleBuffer c) (Seq c) (Seq c)
uncommitted = Optics.lens (\(DoubleBuffer x _) -> x) \(DoubleBuffer _ y) x -> DoubleBuffer x y
unviewed    = Optics.lens (\(DoubleBuffer _ y) -> y) \(DoubleBuffer x _) y -> DoubleBuffer x y

{-| Turn a pushback block producer into a cursor feed

When this cursor receives a 'Reset' request, any input that has been read from
upstream but not committed is pushed back up to the source. -}
pushback :: forall up block action mode.
    Block block => PushbackStream block up =>
    FeedPlus up action mode block
pushback = start :: FeedPlus up action mode block
  where
    start = go (DoubleBuffer mempty mempty)

    go :: DoubleBuffer block -> FeedPlus up action mode block
    go s = Vendor \case
        Reset -> do
            Seq.reverse (view uncommitted s) & traverse_ \b -> Job.order (push b)
            pure $ Referral () start
        Next -> do
            (xm, s') <- State.runStateT (maybeAlt nextFromBuffer nextFromUpstream) s
            pure $ Referral (maybe End Item xm) (go s')
        Commit n -> do
            (r, s') <- State.runStateT (commitAlternative commitFromBuffer commitFromUpstream n) s
            pure $ Referral r (go s')

nextFromBuffer :: StateT (DoubleBuffer block) (Job up action) (Maybe block)
nextFromBuffer = use unviewed >>= \case
    Seq.Empty -> pure Nothing
    x :<| xs -> assign unviewed xs $> Just x

nextFromUpstream :: TerminableStream block up =>
    StateT (DoubleBuffer block) (Job up action) (Maybe block)
nextFromUpstream = lift (Job.order next) >>= \case
    End -> pure Nothing
    Item x -> modifying uncommitted (:|> x) $> Just x

commitFromBuffer :: Block block =>
    Positive -> StateT (DoubleBuffer block) (Job up action) Advancement
commitFromBuffer n = use uncommitted >>= \case
    Seq.Empty -> pure YouCanNotAdvance{ shortfall = n }
    x :<| xs -> case drop n x of
        DropAll -> assign uncommitted xs $> AdvanceSuccess
        DropPart{ dropRemainder = x' } -> assign uncommitted (x' :<| xs) $> AdvanceSuccess
        DropInsufficient{ dropShortfall = n' } -> assign uncommitted xs *> commitFromBuffer n'

commitFromUpstream :: Block block => TerminableStream block up =>
    Positive -> StateT (DoubleBuffer block) (Job up action) Advancement
commitFromUpstream n = lift (Job.order next) >>= \case
    End -> pure YouCanNotAdvance{ shortfall = n }
    Item x -> do
        modifying unviewed (:|> x)
        case drop n x of
            DropAll -> pure AdvanceSuccess
            DropPart{ dropRemainder = x' } -> do
                assign uncommitted (Seq.singleton x')
                pure AdvanceSuccess
            DropInsufficient{ dropShortfall = n' } -> commitFromUpstream n'

maybeAlt :: Monad m => m (Maybe a) -> m (Maybe a) -> m (Maybe a)
maybeAlt a b = a >>= \case
    Just x -> pure (Just x)
    Nothing -> b

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

privateBuffer :: Block block => TerminableStream block up =>
    FeedPlus up action mode block
privateBuffer = Pushback.Buffer.private >-> pushback
