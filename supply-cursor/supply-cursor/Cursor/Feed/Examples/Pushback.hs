module Cursor.Feed.Examples.Pushback
  (
    {- * Feed -} pushback,
    {- * Internals -} Buffer (..), BufferLens,
            uncommitted, unviewed,
            nextFromBuffer, nextFromUpstream,
            commitFromBuffer, commitFromUpstream,
  )
  where

import Essentials
import Cursor.Feed.Type
import Cursor.Interface

import Block.Class (Block)
import Block.Class (Drop (..), drop)
import Control.Monad.State (StateT)
import Control.Monad.Trans (lift)
import Data.Sequence (Seq (..))
import Integer (Positive)
import Next (TerminableStream)
import Optics (assign, use, modifying, view)
import Pushback.Interface (PushbackStream, push)
import SupplyChain (Job, Vendor (Vendor), Referral (Referral))

import qualified Control.Monad.State as State
import qualified Data.Sequence as Seq
import qualified Optics
import qualified SupplyChain.Job as Job

{-| The state used internally by 'pushback' -}
data Buffer block = Buffer
    { bufferUncommitted :: Seq block -- ^ 'uncommitted'
    , bufferUnviewed    :: Seq block -- ^ 'unviewed'
    }

type BufferLens block =
    Optics.Lens (Buffer block) (Buffer block) (Seq block) (Seq block)

{-| Input that has been obtained from upstream but not committed -}
uncommitted :: BufferLens block

{-| Input that has been obtained from upstream but not viewed
    since the last 'Reset' -}
unviewed :: BufferLens block

uncommitted = Optics.lens bufferUncommitted \s a -> s{ bufferUncommitted = a }
unviewed    = Optics.lens bufferUnviewed    \s a -> s{ bufferUnviewed    = a }

{-| Turn a pushback block producer into a cursor feed

Behaviors:

- 'Next'   - First try 'nextFromBuffer'. If that fails because the 'unviewed'
             is empty, then try 'nextFromUpstream'.
- 'Commit' - First try 'commitFromBuffer'. If the 'uncommitted' buffer is
             insufficient to complete the commit, then try 'commitFromUpstream'.
- 'Reset'  - Push any 'uncommitted' input back upstream, then empty both buffers. -}
pushback :: forall up block action mode. PushbackStream block up =>
    FeedPlus up action mode block
pushback = start :: FeedPlus up action mode block
  where
    start = go (Buffer mempty mempty)

    go :: Buffer block -> FeedPlus up action mode block
    go s = Vendor \case
        Next -> do
            (xm, s') <- State.runStateT (maybeAlternative nextFromBuffer nextFromUpstream) s
            pure $ Referral (maybe End Item xm) (go s')
        Commit n -> do
            (r, s') <- State.runStateT (commitAlternative commitFromBuffer commitFromUpstream n) s
            pure $ Referral r (go s')
        Reset -> do
            Seq.reverse (view uncommitted s) & traverse_ \b -> Job.order (push b)
            pure $ Referral () start

{-| Tries to pop the head off of the 'unviewed' buffer -}
nextFromBuffer :: StateT (Buffer block) (Job up action) (Maybe block)
nextFromBuffer = use unviewed >>= \case
    Seq.Empty -> pure Nothing
    x :<| xs -> assign unviewed xs $> Just x

{-| Assuming the 'unviewed' buffer is empty, tries to fetch a new block from upstream

If a new block is obtained, it is appended to the 'uncommitted' buffer. -}
nextFromUpstream :: TerminableStream block up =>
    StateT (Buffer block) (Job up action) (Maybe block)
nextFromUpstream = lift (Job.order next) >>= \case
    End -> pure Nothing
    Item x -> modifying uncommitted (:|> x) $> Just x

{-| Tries to pop some fixed number of items from the 'uncommitted' buffer -}
commitFromBuffer :: Block block =>
    Positive -- ^ How many items to commit
    -> StateT (Buffer block) (Job up action) Advancement
commitFromBuffer n = use uncommitted >>= \case
    Seq.Empty -> pure YouCanNotAdvance{ shortfall = n }
    x :<| xs -> case drop n x of
        DropAll -> assign uncommitted xs $> AdvanceSuccess
        DropPart{ dropRemainder = x' } -> assign uncommitted (x' :<| xs) $> AdvanceSuccess
        DropInsufficient{ dropShortfall = n' } -> assign uncommitted xs *> commitFromBuffer n'

{- | Assuming the 'uncommitted' buffer is empty, fetches more blocks from
     upstream to commit

All new blocks obtained from upstream are appended to the 'unviewed' buffer.
If a block is obtained from upstream and only partially committed, its
remainder becomes the new content of the 'uncommitted' buffer. -}
commitFromUpstream :: Block block => TerminableStream block up =>
    Positive -> StateT (Buffer block) (Job up action) Advancement
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

maybeAlternative :: Monad m => m (Maybe a) -> m (Maybe a) -> m (Maybe a)
maybeAlternative a b = a >>= \case
    Just x -> pure (Just x)
    Nothing -> b
