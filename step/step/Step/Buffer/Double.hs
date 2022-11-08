module Step.Buffer.Double (doubleBuffer) where

import Step.Chunk
import Step.Interface
import qualified Step.Interface.Core as I
import Step.Buffer.Buffer

-- The basics
import Data.Bool (Bool (..))
import Data.Maybe (Maybe (..), isNothing)
import Data.Functor (Functor (..), (<&>), ($>), (<$>))
import Data.Function (($))
import Control.Monad (Monad (..))
import Control.Applicative (Applicative (..))

-- Containers
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

-- Optics
import Optics (Lens', use, assign, modifying)

-- Math
import Numeric.Natural (Natural)
import NatOptics.Positive.Unsafe (Positive)

-- Transformers
import Control.Monad.State.Strict (MonadState)

-- Streaming
import SupplyChain (Vendor (..), Job, Referral (..), (>->), order)
import SupplyChain.Interface.TerminableStream (IsTerminableStream, TerminableStream)
import qualified SupplyChain
import qualified SupplyChain.Interface.TerminableStream as Stream

data DoubleBuffer c = DoubleBuffer{ commitBuffer :: Buffer c, viewBuffer :: Buffer c }

doubleBuffer :: forall c up action. Chunk c => IsTerminableStream c up =>
    (Buffer c -> Job up action ()) ->
    Buffer c -> Vendor up (CommittableChunkStream c) action
doubleBuffer report b = go (DoubleBuffer b b)
  where
    go s = Vendor \case
        I.Reset -> pure $ Referral () $ go s{ viewBuffer = commitBuffer s }
        I.NextMaybe -> case viewBuffer s of
            x :< xs -> pure $ Referral (Just x) $ go s{ viewBuffer = xs }
            Empty -> SupplyChain.order Stream.nextMaybe >>= \case
                Nothing -> pure $ Referral Nothing $ go s
                Just x -> report com $> Referral (Just x) (go s{ commitBuffer = com })
                  where com = commitBuffer s :> x
        I.Commit n -> handleCommit s n

    handleCommit :: DoubleBuffer c -> Positive Natural
        -> Job up action (Referral up (CommittableChunkStream c) action AdvanceResult)
    handleCommit s n = case commitBuffer s of
        x :< xs -> case drop n x of
            DropAll -> report xs $> Referral AdvanceSuccess (go s{ commitBuffer = xs })
            DropPart{ dropRemainder = x' } ->
                report com $> Referral AdvanceSuccess (go s{ commitBuffer = com })
              where
                com = x' :< xs
            DropInsufficient{ dropShortfall = n' } -> report xs *> handleCommit s{ commitBuffer = xs } n'
        Empty -> SupplyChain.order Stream.nextMaybe >>= \case
            Nothing -> pure $ Referral YouCanNotAdvance{ shortfall = n } $ go s
            Just x -> report com *> handleCommit s{ commitBuffer = com, viewBuffer = viewBuffer s :> x } n
              where
                com = One x


