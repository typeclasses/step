module Step.Package.Match (match) where

import Step.Action.Core
import Step.Package.Failure
import Step.Chunk
import Step.Interface
import qualified Step.Interface.Core as I
import Step.Package.FixedLength (tryTakeNatural)

import qualified Step.Do as P
import qualified Step.Interface as Interface

import Data.Kind (Type)
import Control.Applicative (pure)
import Control.Monad ((>>=))
import Data.Bool (Bool (..))
import Data.Either (Either (..))
import Data.Function
import Data.Maybe (Maybe (..))
import Numeric.Natural
import SupplyChain (Vendor (..), Job, Referral (..), (>->))
import Data.Functor
import Prelude ((+), (-))
import Data.Foldable (traverse_)
import Data.Sequence (Seq (..))
import NatOptics.Positive (Positive)

import qualified Data.Sequence as Seq
import qualified Optics
import qualified NatOptics.Positive as Positive

import qualified SupplyChain
import qualified SupplyChain.Job as Job
import qualified SupplyChain.Vendor as Vendor
import qualified SupplyChain.Interface.TerminableStream as Stream
import SupplyChain.Interface.TerminableStream (IsTerminableStream)

match :: Chunk c => Any c m r e a -> Any c m r e (Maybe c, a)
match (Any x) =
  P.do
    (n, a) <- act @Any \r -> counting >-> do
        ea <- Vendor.map Order >-> resettingSequenceJob (x r)
        n <- SupplyChain.order AmountCommitted
        pure case ea of
            Left e -> Left e
            Right a -> Right (n, a)
    c <- tryTakeNatural n
    P.pure (c, a)

data Counting (c :: Type) (response :: Type) =
    Order (CommittableChunkStream c response)
  | (response ~ Natural) => AmountCommitted

data State c = State
  { committed :: Natural
  , commitBuffer :: Seq c
  , viewBuffer :: Seq c
  }

counting :: forall c up action. Chunk c => IsTerminableStream c up =>
    Vendor up (Counting c) action
counting = go State{ committed = 0, commitBuffer = Seq.empty, viewBuffer = Seq.empty }
  where
    go :: State c -> Vendor up (Counting c) action
    go s = Vendor \case
        AmountCommitted -> pure $ Referral (committed s) $ go s
        Order I.Reset -> pure $ Referral () $ go s{ viewBuffer = commitBuffer s }

        Order I.NextMaybe ->
            case viewBuffer s of
                x :<| xs -> pure $ Referral (Just x) $ go s{ viewBuffer = xs }
                Empty -> SupplyChain.order Stream.nextMaybe >>= \case
                    Nothing -> pure $ Referral Nothing $ go s
                    Just x -> pure $ Referral (Just x) $ go s{ commitBuffer = commitBuffer s :|> x }

        Order c@(I.Commit n) ->
            case commitBuffer s of
                x :<| xs -> case drop n x of
                    DropAll ->
                        pure $ Referral AdvanceSuccess $ go
                          s { commitBuffer = xs
                            , committed = committed s + g n
                            }
                    DropPart{ dropRemainder = x' } ->
                        pure $ Referral AdvanceSuccess $ go
                          s { commitBuffer = x' :<| xs
                            , committed = committed s + g n
                            }
                    DropInsufficient{ dropShortfall = n' } ->
                      let
                        s' = s { commitBuffer = xs
                               , committed = committed s + g (length x)
                               }
                      in
                        go s' `handle` Order (I.Commit n')
                Empty -> SupplyChain.order Stream.nextMaybe >>= \case
                    Nothing -> pure $ Referral YouCanNotAdvance{ shortfall = n } $ go s
                    Just x ->
                      let
                        s' = s { commitBuffer = Seq.singleton x
                               , viewBuffer = viewBuffer s :|> x
                               }
                      in
                        go s' `handle` Order c

g :: Positive Natural -> Natural
g = Optics.review Positive.refine
