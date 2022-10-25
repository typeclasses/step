module Step.Package.ParticularText
  (
    nextTextIs, takeParticularText, takeParticularTextAtomic,
    nextTextMatchesOn, takeMatchingText, takeMatchingTextAtomic,
  )
  where

import Step.Action.Core
import Step.Chunk
import Step.Error
import Step.Interface
import Step.Package.FixedLength
import Step.Package.Failure
import Step.Interface

import qualified Step.Do as P
import qualified Step.Interface as Interface

import Control.Applicative (pure, (*>))
import Control.Monad ((>>=))
import Data.Bool (Bool (..))
import Data.Either (Either (..))
import Data.Eq (Eq, (==))
import Data.Function (($))
import Data.Functor ((<&>), fmap)
import Data.Maybe (Maybe (..))
import SupplyChain (Job, perform, order, noVendor, (>->))
import Data.List.NonEmpty (NonEmpty ((:|)))

import qualified SupplyChain

takeParticularText :: forall c m e. Chunk c => Eq c => ErrorContext e m => c -> Move c m e ()
takeParticularText = \t -> assumeMovement $
    Any (ResettingSequence (go t) <&> Right) P.>>= requireTrue
  where
    go :: c -> Job (CommittableChunkStream c) m Bool
    go t = order nextMaybe >>= \case
        Nothing -> pure False
        Just x -> case stripEitherPrefix (ChunkCharacterEquivalence (==)) x t of
            StripEitherPrefixFail         ->  pure False
            StripEitherPrefixAll          ->  order (commit (length t)) <&> \_ -> True
            IsPrefixedBy{}                ->  order (commit (length t)) <&> \_ -> True
            IsPrefixOf{ extraPart = t' }  ->  order (commit (length x)) *> go t'

nextTextIs :: forall c m e. Chunk c => Eq c => c -> SureQuery c m e Bool
nextTextIs = nextTextMatchesOn (ChunkCharacterEquivalence (==))

takeParticularTextAtomic :: forall c m e. Chunk c => Eq c =>
    ErrorContext e m => c -> AtomicMove c m e ()
takeParticularTextAtomic t = assumeMovement $
    (nextTextIs t P.>>= requireTrue) P.<* trySkipPositive (length t)

nextTextMatchesOn :: forall c m e. Chunk c =>
    ChunkCharacterEquivalence c -> c -> SureQuery c m e Bool
nextTextMatchesOn eq = \t -> SureQuery (ResettingSequence (go t))
  where
    go :: c -> Job (ResettableTerminableStream c) m Bool
    go t = order nextMaybe >>= \case
        Nothing -> pure False
        Just x -> case stripEitherPrefix eq x t of
            StripEitherPrefixFail         ->  pure False
            StripEitherPrefixAll          ->  pure True
            IsPrefixedBy{}                ->  pure True
            IsPrefixOf{ extraPart = t' }  ->  go t'

takeMatchingText :: forall c m e. ErrorContext e m => Chunk c =>
    ChunkCharacterEquivalence c -> c -> Move c m e c
takeMatchingText eq = \t -> assumeMovement $ Any $ ResettingSequence $ fmap (fmap concat) $ go t
  where
    go :: c -> Job (CommittableChunkStream c) m (Either e (NonEmpty c))
    go t = order nextMaybe >>= \case
        Nothing -> (noVendor >-> getError) <&> Left
        Just x -> case stripEitherPrefix eq x t of
            StripEitherPrefixFail            ->  (noVendor >-> getError) <&> Left
            StripEitherPrefixAll             ->  pure $ Right $ x :| []
            IsPrefixedBy{ commonPart = x' }  ->  pure $ Right $ x' :| []
            IsPrefixOf{ extraPart = t' }     ->  go t'

takeMatchingTextAtomic :: forall c m e. Chunk c => ErrorContext e m =>
    ChunkCharacterEquivalence c -> c -> AtomicMove c m e c
takeMatchingTextAtomic eq t =
    (nextTextMatchesOn eq t P.>>= requireTrue) P.*> takePositiveAtomic (length t)
