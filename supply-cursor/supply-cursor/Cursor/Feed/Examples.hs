module Cursor.Feed.Examples
  (
    produceFromState,
    pipe,
    pipeWithBufferState,
    pipeWithBufferReporting,
  )
  where

import Essentials
import Cursor.Feed.Type
import Cursor.Interface

import Block.Class (Block)
import Block.Class (Drop (..), drop)
import Control.Monad.State (MonadState)
import Data.Sequence (Seq (..))
import Integer (Positive)
import Next (TerminableStream, Step (..))
import Optics (Lens', use, assign)
import SupplyChain (Job, Vendor (Vendor), order, Referral (Referral), perform, (>->))

import qualified SupplyChain.Vendor as Vendor
import qualified Data.Sequence as Seq
import qualified Next

{-| In-memory cursor feed that obtains its input from state -}
produceFromState :: forall state up action block.
    Block block => MonadState state action =>
    Lens' state (Seq block) -> FeedPlus up action 'Write block
produceFromState buffer = Next.empty >-> pipeWithBufferState buffer

{-| Turn a block producer into a cursor feed -}
pipe :: forall block up action. Block block => TerminableStream block up =>
    FeedPlus up action 'Write block
pipe = pipeWithBufferReporting (\_ -> pure ()) Empty

{-| Turn a block producer into a cursor feed, using state to store remainders

A buffer stored in the 'MonadState' context, at a position identified by the
given lens parameter, holds any input that has been read from the unbuffered
stream but has not yet been committed. The remaining input, then, consists of
anything that is in the buffer, followed by anything that is yet to be obtained
from the unbuffered stream. -}
pipeWithBufferState :: forall state up action block.
    Block block => MonadState state action => TerminableStream block up =>
    Lens' state (Seq block) -> Vendor up (Cursor 'Write block) action
pipeWithBufferState buffer = Vendor \request -> do
    b <- perform $ use buffer
    Vendor.handle (pipeWithBufferReporting report b) request
  where
    report b = perform (assign buffer b)

data DoubleBuffer c = DoubleBuffer{ commitBuffer :: Seq c, viewBuffer :: Seq c }

{-| Turn a block producer into a cursor feed, with manual handling of remainders -}
pipeWithBufferReporting :: forall block up action.
    Block block => TerminableStream block up =>
    (Seq block -> Job up action ()) -- ^ Called whenever the buffer changes
    -> Seq block -- ^ Initial buffer
    -> FeedPlus up action 'Write block
pipeWithBufferReporting report b = go (DoubleBuffer b b)
  where
    go s = Vendor \case
        Reset -> pure $ Referral () $ go s{ viewBuffer = commitBuffer s }
        Next -> case viewBuffer s of
            x :<| xs -> pure $ Referral (Item x) $ go s{ viewBuffer = xs }
            Empty -> order Next.next >>= \case
                End -> pure $ Referral End $ go s
                Item x -> report com $> Referral (Item x) (go s{ commitBuffer = com })
                  where com = commitBuffer s :|> x
        Commit n -> handleCommit s n

    handleCommit :: DoubleBuffer block -> Positive
        -> Job up action (Referral up (Cursor 'Write block) action Advancement)
    handleCommit s n = case commitBuffer s of
        x :<| xs -> case drop n x of
            DropAll -> report xs $> Referral AdvanceSuccess (go s{ commitBuffer = xs })
            DropPart{ dropRemainder = x' } ->
                report com $> Referral AdvanceSuccess (go s{ commitBuffer = com })
              where
                com = x' :<| xs
            DropInsufficient{ dropShortfall = n' } -> report xs *> handleCommit s{ commitBuffer = xs } n'
        Empty -> order Next.next >>= \case
            End -> pure $ Referral YouCanNotAdvance{ shortfall = n } $ go s
            Item x -> report com *> handleCommit s{ commitBuffer = com, viewBuffer = viewBuffer s :|> x } n
              where
                com = Seq.singleton x
