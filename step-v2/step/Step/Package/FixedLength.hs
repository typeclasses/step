module Step.Package.FixedLength
  (
    trySkipPositive, trySkipPositive_, skipPositive,
    trySkipNatural, trySkipNatural_, skipNatural,
    takePositive, takePositiveAtomic, peekPositive,
    skipPositiveAtomic, skipNaturalAtomic,
    remainsAtLeastPositive, remainsAtLeastNatural,
    ensureAtLeastPositive, ensureAtLeastNatural,
    tryTakeNatural, tryTakePositive,
  )
  where

import Essentials
import Step.Action.Core
import Block.Class
import Step.Interface
import Step.Package.Failure
import Step.LeftRight

import qualified Step.Do as P

import Numeric.Natural (Natural)
import Integer (Positive)
import SupplyChain (Job, order)
import Data.List.NonEmpty (NonEmpty ((:|)))

import qualified Data.List.NonEmpty as NE
import qualified Integer.Positive as Positive
import qualified Integer.Signed as Signed

ifZero :: a -> (Positive -> a) -> Natural -> a
ifZero z p n = Positive.fromNatural n & maybe z p

{-| Advance over the next /n/ characters (if possible)

    If the end of input is reached in fewer than /n/ characters,
    cursor advances to the end of input, and the returned
    'AdvanceResult' gives the size of the difference.
-}
trySkipPositive :: forall c m r. Positive -> Sure c m r AdvanceResult
trySkipPositive n = Sure \_ -> ResettingSequenceJob $ order $ commit n

trySkipPositive_ :: Positive -> Sure c m r ()
trySkipPositive_ = void . trySkipPositive

trySkipNatural :: forall c m r. Natural -> Sure c m r AdvanceResult
trySkipNatural = ifZero (pure' AdvanceSuccess) trySkipPositive

trySkipNatural_ :: forall c m r. Natural -> Sure c m r ()
trySkipNatural_ = ifZero (pure' ()) trySkipPositive_

tryTakeNatural :: forall c m r. Block c => Natural -> Sure c m r (Maybe c)
tryTakeNatural = ifZero (pure' Nothing) tryTakePositive

skipPositive :: forall c m r. Positive -> Any c m r ()
skipPositive n = trySkipPositive n P.>>= requireAdvanceSuccess

skipNatural :: forall c m r. Natural -> Any c m r ()
skipNatural = ifZero (pure' ()) $ cast . skipPositive

remainsAtLeastPositive :: forall c m r. Block c => Positive -> SureQuery c m r Bool
remainsAtLeastPositive = \n -> act @SureQuery \r -> fmap right $ go r n
  where
    go :: r -> Positive -> Job (ResettableTerminableStream c) m Bool
    go r n = order next >>= \case
        End -> pure False
        Item x -> case Positive.subtract n (length @c x) of
            Signed.Plus n' -> go r n'
            _ -> pure True

remainsAtLeastNatural :: forall c m r. Block c =>
    Natural -> SureQuery c m r Bool
remainsAtLeastNatural = ifZero (pure' True) remainsAtLeastPositive

ensureAtLeastPositive :: forall c m r. Block c => Positive -> Query c m r ()
ensureAtLeastPositive n = remainsAtLeastPositive n P.>>= requireTrue

ensureAtLeastNatural :: forall c m r. Block c => Natural -> Query c m r ()
ensureAtLeastNatural = ifZero (pure' ()) ensureAtLeastPositive

skipPositiveAtomic :: forall c m r. Block c => Positive -> Atom c m r ()
skipPositiveAtomic n = ensureAtLeastPositive n P.<* trySkipPositive n

skipNaturalAtomic :: forall c m r. Block c => Natural -> Atom c m r ()
skipNaturalAtomic = ifZero (pure' ()) $ cast . skipPositiveAtomic

peekPositive :: forall c m r. Block c => Positive -> Query c m r c
peekPositive = \n -> Query $ required $ go n
  where
    go :: Positive -> Job (ResettableTerminableStream c) m (Maybe (NonEmpty c))
    go n = order next >>= \case
        End -> pure Nothing
        Item x -> case take n x of
            TakeAll -> pure $ Just $ x :| []
            TakePart{ takePart } -> pure $ Just $ takePart :| []
            TakeInsufficient{ takeShortfall } -> fmap (NE.cons x) <$> go takeShortfall

takePositive :: forall c m r. Block c => Positive -> Any c m r c
takePositive = \n -> Any $ required $ go n
  where
    go :: Positive -> Job (CommittableChunkStream c) m (Maybe (NonEmpty c))
    go n = order next >>= \case
        End -> pure Nothing
        Item x -> case take n x of
            TakeAll -> order (commit n) $> Just (x :| [])
            TakePart{ takePart } -> order (commit (length takePart)) $> Just (takePart :| [])
            TakeInsufficient{ takeShortfall } -> fmap (NE.cons x) <$> go takeShortfall

tryTakePositive :: forall c m r. Block c => Positive -> Sure c m r (Maybe c)
tryTakePositive = \n -> act @Sure \_ -> fmap (right . fmap concat . NE.nonEmpty) $ go n
  where
    go :: Positive -> Job (CommittableChunkStream c) m [c]
    go n = order next >>= \case
        End -> pure []
        Item x -> case take n x of
            TakeAll -> order (commit n) $> [x]
            TakePart{ takePart } -> order (commit (length takePart)) $> [takePart]
            TakeInsufficient{ takeShortfall } -> order (commit (length x)) *> ((x :) <$> go takeShortfall)

required :: Block b => Job up action (Maybe (NonEmpty b)) -> a -> ResettingSequence up action (Either a b)
required go r = ResettingSequenceJob $ go <&> maybe (Left r) Right <&> fmap concat

takePositiveAtomic :: forall c m r. Block c => Positive -> Atom c m r c
takePositiveAtomic = \n -> peekPositive n P.<* trySkipPositive n
