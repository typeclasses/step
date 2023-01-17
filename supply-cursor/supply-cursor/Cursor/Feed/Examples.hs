module Cursor.Feed.Examples where

import Essentials
import Cursor.Feed.Type
import Cursor.Interface

import Data.Sequence (Seq)
import Integer (Positive)
import Next (TerminableStream, Step (..))
import Block.Class (Block)
import SupplyChain (Job, Vendor (Vendor), order, Referral (Referral))
import Data.Sequence (Seq (..))
import Block.Class (Drop (..), drop)

import qualified Data.Sequence as Seq
import qualified Next

data DoubleBuffer c = DoubleBuffer{ commitBuffer :: Seq c, viewBuffer :: Seq c }

reportedBuffer :: forall block up action. Block block => TerminableStream block up =>
    (Seq block -> Job up action ()) -- ^ Called whenever the buffer changes
    -> Seq block -- ^ Initial buffer
    -> FeedPlus up action 'Write block
reportedBuffer report b = go (DoubleBuffer b b)
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

privateBuffer :: forall block up action. Block block => TerminableStream block up =>
    FeedPlus up action 'Write block
privateBuffer = reportedBuffer (\_ -> pure ()) Empty
