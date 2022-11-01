module Step.Package.FixedLength
  (
    trySkipPositive, skipPositive, trySkipNatural, skipNatural,
    takePositive, takePositiveAtomic, peekPositive,
    skipPositiveAtomic, skipNaturalAtomic,
    remainsAtLeastPositive, remainsAtLeastNatural,
    ensureAtLeastPositive, ensureAtLeastNatural,
  )
  where

import Step.Action.Core
import Step.Chunk
import Step.Interface
import Step.Package.Failure
import Step.Interface

import qualified Step.Do as P
import qualified Step.Interface as Interface

import Control.Applicative (pure)
import Control.Monad ((>>=))
import Data.Bool (Bool (..))
import Data.Either (Either (..))
import Data.Function (($))
import Data.Functor (($>), (<&>), (<$>), fmap)
import Data.Maybe (Maybe (..))
import Numeric.Natural (Natural)
import NatOptics.Positive.Unsafe (Positive)
import SupplyChain (Job, perform, order, absurdOrder, (>->))
import Data.List.NonEmpty (NonEmpty ((:|)))

import qualified Data.List.NonEmpty as NE
import qualified NatOptics.Positive as Positive
import qualified NatOptics.Positive.Math as Positive
import qualified NatOptics.Signed as Signed
import qualified Optics
import qualified SupplyChain

{-| Advance over the next /n/ characters (if possible)

    If the end of input is reached in fewer than /n/ characters,
    cursor advances to the end of input, and the returned
    'AdvanceResult' gives the size of the difference.
-}
trySkipPositive :: forall c m r. Positive Natural -> Sure c m r r AdvanceResult
trySkipPositive n = Sure (ResettingSequence (order (commit n)))

trySkipNatural :: forall c m r. Natural -> Sure c m r r AdvanceResult
trySkipNatural n = case Optics.preview Positive.refine n of
    Nothing  ->  pure AdvanceSuccess
    Just p   ->  trySkipPositive p

skipPositive :: forall c m r. Positive Natural -> Move c m r r ()
skipPositive n = assumeMovement $ trySkipPositive n P.>>= \case
    AdvanceSuccess      ->  pure ()
    YouCanNotAdvance{}  ->  castTo @Any fail

skipNatural :: forall c m r. Natural -> Any c m r r ()
skipNatural n = case Optics.preview Positive.refine n of
    Nothing  ->  pure ()
    Just p   ->  castTo @Any (skipPositive p)

remainsAtLeastPositive :: forall c m r. Chunk c =>
    Positive Natural -> SureQuery c m r r Bool
remainsAtLeastPositive = \n -> act @SureQuery (go n)
  where
    go :: Positive Natural -> Job (ResettableTerminableStream c) m r Bool
    go n = order nextMaybe >>= \case
        Nothing -> pure False
        Just x -> case Positive.minus n (length @c x) of
            Signed.Plus n' -> go n'
            _ -> pure True

remainsAtLeastNatural :: forall c m r. Chunk c =>
    Natural -> SureQuery c m r r Bool
remainsAtLeastNatural n = case Optics.preview Positive.refine n of
    Nothing  ->  pure True
    Just p   ->  remainsAtLeastPositive p

ensureAtLeastPositive :: forall c m r. Chunk c =>
    Positive Natural -> Query c m r r ()
ensureAtLeastPositive n = remainsAtLeastPositive n P.>>= \case
    False  ->  castTo @Query fail
    True   ->  pure ()

ensureAtLeastNatural :: forall c m r. Chunk c =>
    Natural -> Query c m r r ()
ensureAtLeastNatural n = case Optics.preview Positive.refine n of
    Nothing  ->  pure ()
    Just p   ->  ensureAtLeastPositive p

skipPositiveAtomic :: forall c m r. Chunk c =>
    Positive Natural -> AtomicMove c m r r ()
skipPositiveAtomic n = assumeMovement $
    ensureAtLeastPositive n P.<* trySkipPositive n

skipNaturalAtomic :: forall c m r. Chunk c =>
    Natural -> Atom c m r r ()
skipNaturalAtomic n = case Optics.preview Positive.refine n of
    Nothing  ->  castTo @Atom (P.pure ())
    Just p   ->  castTo @Atom (skipPositiveAtomic p)

peekPositive :: forall c m r. Chunk c =>
    Positive Natural -> Query c m r r c
peekPositive = \n -> Query $ ResettingSequence $ fmap (fmap concat) $ go n
  where
    go :: Positive Natural -> Job (ResettableTerminableStream c) m r (Either r (NonEmpty c))
    go n = order nextMaybe >>= \case
        Nothing -> SupplyChain.param <&> Left
        Just x -> case take n x of
            TakeAll -> pure $ Right $ x :| []
            TakePart{ takePart } -> pure $ Right $ takePart :| []
            TakeInsufficient{ takeShortfall } -> fmap (NE.cons x) <$> go takeShortfall

takePositive :: forall c m r. Chunk c =>
    Positive Natural -> Move c m r r c
takePositive = \n -> assumeMovement $ Any $ ResettingSequence $ fmap (fmap concat) $ go n
  where
    go :: Positive Natural -> Job (CommittableChunkStream c) m r (Either r (NonEmpty c))
    go n = order nextMaybe >>= \case
        Nothing -> SupplyChain.param <&> Left
        Just x -> case take n x of
            TakeAll -> order (commit n) $> Right (x :| [])
            TakePart{ takePart } -> order (commit (length takePart)) $> Right (takePart :| [])
            TakeInsufficient{ takeShortfall } -> fmap (NE.cons x) <$> go takeShortfall

takePositiveAtomic :: forall c m r. Chunk c =>
    Positive Natural -> AtomicMove c m r r c
takePositiveAtomic = \n -> assumeMovement $ peekPositive n P.<* trySkipPositive n
