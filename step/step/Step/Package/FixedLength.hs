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

import Step.Action.Core
import Step.Chunk
import Step.Interface
import Step.Package.Failure

import qualified Step.Do as P

import Control.Applicative (pure, (*>))
import Control.Monad ((>>=))
import Data.Bool (Bool (..))
import Data.Either (Either (..))
import Data.Function (($), (&), (.))
import Data.Functor (($>), (<&>), (<$>), fmap, void)
import Data.Maybe (Maybe (..), maybe)
import Numeric.Natural (Natural)
import NatOptics.Positive.Unsafe (Positive)
import SupplyChain (Job, order)
import Data.List.NonEmpty (NonEmpty ((:|)))

import qualified Data.List.NonEmpty as NE
import qualified NatOptics.Positive as Positive
import qualified NatOptics.Positive.Math as Positive
import qualified NatOptics.Signed as Signed
import qualified Optics

ifZero :: a -> (Positive Natural -> a) -> Natural -> a
ifZero z p n = Optics.preview Positive.refine n & maybe z p

{-| Advance over the next /n/ characters (if possible)

    If the end of input is reached in fewer than /n/ characters,
    cursor advances to the end of input, and the returned
    'AdvanceResult' gives the size of the difference.
-}
trySkipPositive :: forall c m r. Positive Natural -> Sure c m r r AdvanceResult
trySkipPositive n = Sure \_ -> ResettingSequenceJob $ order $ commit n

trySkipPositive_ :: Positive Natural -> Sure c m r r ()
trySkipPositive_ = void . trySkipPositive

trySkipNatural :: forall c m r. Natural -> Sure c m r r AdvanceResult
trySkipNatural = ifZero (pure' AdvanceSuccess) trySkipPositive

trySkipNatural_ :: forall c m r. Natural -> Sure c m r r ()
trySkipNatural_ = ifZero (pure' ()) trySkipPositive_

tryTakeNatural :: forall c m r e. Chunk c => Natural -> Sure c m r e (Maybe c)
tryTakeNatural = ifZero (pure' Nothing) tryTakePositive

skipPositive :: forall c m r. Positive Natural -> Move c m r r ()
skipPositive n = assumeMovement $ trySkipPositive n P.>>= requireAdvanceSuccess

skipNatural :: forall c m r. Natural -> Any c m r r ()
skipNatural = ifZero (pure' ()) $ cast . skipPositive

remainsAtLeastPositive :: forall c m r. Chunk c => Positive Natural -> SureQuery c m r r Bool
remainsAtLeastPositive = \n -> act @SureQuery \r -> go r n
  where
    go :: r -> Positive Natural -> Job (ResettableTerminableStream c) m Bool
    go r n = order nextMaybe >>= \case
        Nothing -> pure False
        Just x -> case Positive.minus n (length @c x) of
            Signed.Plus n' -> go r n'
            _ -> pure True

remainsAtLeastNatural :: forall c m r. Chunk c =>
    Natural -> SureQuery c m r r Bool
remainsAtLeastNatural = ifZero (pure' True) remainsAtLeastPositive

ensureAtLeastPositive :: forall c m r. Chunk c => Positive Natural -> Query c m r r ()
ensureAtLeastPositive n = remainsAtLeastPositive n P.>>= requireTrue

ensureAtLeastNatural :: forall c m r. Chunk c => Natural -> Query c m r r ()
ensureAtLeastNatural = ifZero (pure' ()) ensureAtLeastPositive

skipPositiveAtomic :: forall c m r. Chunk c => Positive Natural -> AtomicMove c m r r ()
skipPositiveAtomic n = assumeMovement $ ensureAtLeastPositive n P.<* trySkipPositive n

skipNaturalAtomic :: forall c m r. Chunk c => Natural -> Atom c m r r ()
skipNaturalAtomic = ifZero (pure' ()) $ cast . skipPositiveAtomic

peekPositive :: forall c m r. Chunk c => Positive Natural -> Query c m r r c
peekPositive = \n -> Query $ required $ go n
  where
    go :: Positive Natural -> Job (ResettableTerminableStream c) m (Maybe (NonEmpty c))
    go n = order nextMaybe >>= \case
        Nothing -> pure Nothing
        Just x -> case take n x of
            TakeAll -> pure $ Just $ x :| []
            TakePart{ takePart } -> pure $ Just $ takePart :| []
            TakeInsufficient{ takeShortfall } -> fmap (NE.cons x) <$> go takeShortfall

takePositive :: forall c m r. Chunk c => Positive Natural -> Move c m r r c
takePositive = \n -> assumeMovement $ Any $ required $ go n
  where
    go :: Positive Natural -> Job (CommittableChunkStream c) m (Maybe (NonEmpty c))
    go n = order nextMaybe >>= \case
        Nothing -> pure Nothing
        Just x -> case take n x of
            TakeAll -> order (commit n) $> Just (x :| [])
            TakePart{ takePart } -> order (commit (length takePart)) $> Just (takePart :| [])
            TakeInsufficient{ takeShortfall } -> fmap (NE.cons x) <$> go takeShortfall

tryTakePositive :: forall c m r e. Chunk c => Positive Natural -> Sure c m r e (Maybe c)
tryTakePositive = \n -> act @Sure \_ -> fmap (fmap concat . NE.nonEmpty) $ go n
  where
    go :: Positive Natural -> Job (CommittableChunkStream c) m [c]
    go n = order nextMaybe >>= \case
        Nothing -> pure []
        Just x -> case take n x of
            TakeAll -> order (commit n) $> [x]
            TakePart{ takePart } -> order (commit (length takePart)) $> [takePart]
            TakeInsufficient{ takeShortfall } -> order (commit (length x)) *> ((x :) <$> go takeShortfall)

required :: Chunk b => Job up action (Maybe (NonEmpty b)) -> a -> ResettingSequence up action (Either a b)
required go r = ResettingSequenceJob $ go <&> maybe (Left r) Right <&> fmap concat

takePositiveAtomic :: forall c m r. Chunk c => Positive Natural -> AtomicMove c m r r c
takePositiveAtomic = \n -> assumeMovement $ peekPositive n P.<* trySkipPositive n
