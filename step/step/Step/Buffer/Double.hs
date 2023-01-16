module Step.Buffer.Double (doubleBuffer) where

import Chunk
import Step.Interface
import Step.Buffer.Buffer
import Essentials

import SupplyChain (Vendor (..), Job, Referral (..), order)
import Next.Interface (TerminableStream)
import Integer (Positive)

import qualified Step.Interface.Core as I
import qualified Next.Interface as Stream

data DoubleBuffer c = DoubleBuffer{ commitBuffer :: Buffer c, viewBuffer :: Buffer c }

doubleBuffer :: forall c up action. Chunk c => TerminableStream c up =>
    (Buffer c -> Job up action ()) ->
    Buffer c -> Vendor up (CommittableChunkStream c) action
doubleBuffer report b = go (DoubleBuffer b b)
  where
    go s = Vendor \case
        I.Reset -> pure $ Referral () $ go s{ viewBuffer = commitBuffer s }
        I.NextMaybe -> case viewBuffer s of
            x :< xs -> pure $ Referral (Item x) $ go s{ viewBuffer = xs }
            Empty -> SupplyChain.order Stream.next >>= \case
                End -> pure $ Referral End $ go s
                Item x -> report com $> Referral (Item x) (go s{ commitBuffer = com })
                  where com = commitBuffer s :> x
        I.Commit n -> handleCommit s n

    handleCommit :: DoubleBuffer c -> Positive
        -> Job up action (Referral up (CommittableChunkStream c) action AdvanceResult)
    handleCommit s n = case commitBuffer s of
        x :< xs -> case drop n x of
            DropAll -> report xs $> Referral AdvanceSuccess (go s{ commitBuffer = xs })
            DropPart{ dropRemainder = x' } ->
                report com $> Referral AdvanceSuccess (go s{ commitBuffer = com })
              where
                com = x' :< xs
            DropInsufficient{ dropShortfall = n' } -> report xs *> handleCommit s{ commitBuffer = xs } n'
        Empty -> SupplyChain.order Stream.next >>= \case
            End -> pure $ Referral YouCanNotAdvance{ shortfall = n } $ go s
            Item x -> report com *> handleCommit s{ commitBuffer = com, viewBuffer = viewBuffer s :> x } n
              where
                com = One x
