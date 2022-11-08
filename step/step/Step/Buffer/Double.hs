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

data DoubleBuffer c = DoubleBuffer{ dbCommit :: Buffer c, dbView :: Buffer c }

doubleBuffer :: forall c up action. Chunk c => IsTerminableStream c up =>
    (Buffer c -> Job up action ()) ->
    Buffer c -> Vendor up (CommittableChunkStream c) action
doubleBuffer report b = go (DoubleBuffer b b)
  where
    go :: Chunk c => IsTerminableStream c up =>
        DoubleBuffer c -> Vendor up (CommittableChunkStream c) action
    go s = Vendor \case
        I.Reset -> handleReset go s
        I.NextMaybe -> handleNext report go s
        I.Commit n -> handleCommit report go s n

handleReset :: (DoubleBuffer c -> Vendor up down action) -> DoubleBuffer c
    -> Job up action (Referral up down action ())
handleReset go s = pure $ Referral () $ go s{ dbView = dbCommit s }

handleNext :: IsTerminableStream c up =>
    (Buffer c -> Job up action ()) -> (DoubleBuffer c -> Vendor up down action)
    -> DoubleBuffer c -> Job up action (Referral up down action (Maybe c))
handleNext report go s =  case dbView s of
    x :< xs -> pure $ Referral (Just x) $ go s{ dbView = xs }
    Empty -> SupplyChain.order Stream.nextMaybe >>= \case
        Nothing -> pure $ Referral Nothing $ go s
        Just x -> report (dbCommit s :> x) $> Referral (Just x) (go s{ dbCommit = dbCommit s :> x })

handleCommit :: Chunk c => IsTerminableStream c up =>
    (Buffer c -> Job up action ())
    -> (DoubleBuffer c -> Vendor up (CommittableChunkStream c) action)
    -> DoubleBuffer c -> Positive Natural
    -> Job up action (Referral up (CommittableChunkStream c) action AdvanceResult)
handleCommit report go s n = case dbCommit s of
      x :< xs -> case drop n x of
          DropAll -> report xs $> Referral AdvanceSuccess (go s{ dbCommit = xs })
          DropPart{ dropRemainder = x' } -> report (x' :< xs) $> Referral AdvanceSuccess (go s{ dbCommit = x' :< xs })
          DropInsufficient{ dropShortfall = n' } -> report xs *> (go s{ dbCommit = xs } `handle` I.Commit n')
      Empty -> SupplyChain.order Stream.nextMaybe >>= \case
          Nothing -> pure $ Referral YouCanNotAdvance{ shortfall = n } $ go s
          Just x -> report (One x) *> (go s{ dbCommit = One x, dbView = dbView s :> x } `handle` I.Commit n)
