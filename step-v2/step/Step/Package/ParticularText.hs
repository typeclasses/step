module Step.Package.ParticularText
  (
    nextTextIs, takeParticularText, takeParticularTextAtomic,
    nextTextMatchesOn, takeMatchingText, takeMatchingTextAtomic,
  )
  where

import Essentials
import Step.Action.Core
import Block.Class.Class
import Step.Interface
import Step.Package.FixedLength
import Step.Package.Failure

import qualified Step.Do as P

import Data.Either (Either (..))
import SupplyChain (Job, order)
import Data.List.NonEmpty (NonEmpty ((:|)))

takeParticularText :: forall c m r. Block c => Eq c => c -> Any c m r ()
takeParticularText = \t ->
    (Any \_ -> ResettingSequenceJob (go t) <&> Right) P.>>= requireTrue
  where
    go :: c -> Job (CommittableChunkStream c) m Bool
    go t = order next >>= \case
        End -> pure False
        Item x -> case stripEitherPrefix (BlockCharacterEquivalence (==)) x t of
            StripEitherPrefixFail         ->  pure False
            StripEitherPrefixAll          ->  order (commit (length t)) <&> \_ -> True
            IsPrefixedBy{}                ->  order (commit (length t)) <&> \_ -> True
            IsPrefixOf{ extraPart = t' }  ->  order (commit (length x)) *> go t'

nextTextIs :: forall c m r. Block c => Eq c => c -> SureQuery c m r Bool
nextTextIs = nextTextMatchesOn (BlockCharacterEquivalence (==))

takeParticularTextAtomic :: forall c m r. Block c => Eq c =>
    c -> Atom c m r ()
takeParticularTextAtomic t =
    (nextTextIs t P.>>= requireTrue) P.<* trySkipPositive (length t)

nextTextMatchesOn :: forall c m r. Block c =>
    BlockCharacterEquivalence c -> c -> SureQuery c m r Bool
nextTextMatchesOn eq = \t -> SureQuery \_ -> ResettingSequenceJob (go t)
  where
    go :: c -> Job (ResettableTerminableStream c) m Bool
    go t = order next >>= \case
        End -> pure False
        Item x -> case stripEitherPrefix eq x t of
            StripEitherPrefixFail         ->  pure False
            StripEitherPrefixAll          ->  pure True
            IsPrefixedBy{}                ->  pure True
            IsPrefixOf{ extraPart = t' }  ->  go t'

takeMatchingText :: forall c m r. Block c =>
    BlockCharacterEquivalence c -> c -> Any c m r c
takeMatchingText eq = \t -> Any \r -> ResettingSequenceJob $ fmap (fmap concat) $ go r t
  where
    go :: r -> c -> Job (CommittableChunkStream c) m (Either r (NonEmpty c))
    go r t = order next >>= \case
        End -> pure (Left r)
        Item x -> case stripEitherPrefix eq x t of
            StripEitherPrefixFail            ->  pure $ Left r
            StripEitherPrefixAll             ->  pure $ Right $ x :| []
            IsPrefixedBy{ commonPart = x' }  ->  pure $ Right $ x' :| []
            IsPrefixOf{ extraPart = t' }     ->  go r t'

takeMatchingTextAtomic :: forall c m r. Block c =>
    BlockCharacterEquivalence c -> c -> Atom c m r c
takeMatchingTextAtomic eq t =
    (nextTextMatchesOn eq t P.>>= requireTrue) P.*> takePositiveAtomic (length t)
