module Step.Walk
  (
    Walk (..),

    commit, peekSomeMaybe, peekCharMaybe, atEnd,
    takeText, nextTextIs,
  )
  where

import Step.Chunk
import Step.Interface.Core
import Step.Walk.Core

import qualified Step.Interface as Interface

import Data.Bool (Bool (..))
import Data.Eq
import Data.Maybe (Maybe (..))
import Data.Functor ((<&>))
import Control.Monad (Monad (..))
import Control.Applicative (Applicative (..))
import Numeric.Natural (Natural)
import NatOptics.Positive.Unsafe (Positive)
import SupplyChain (Factory)
import Data.Functor.Contravariant

commit :: forall c m. Positive Natural -> Walk 'RW c m AdvanceResult
commit n = Walk (Interface.commit n)

peekSomeMaybe :: forall c m mode. Walk mode c m (Maybe c)
peekSomeMaybe = Walk Interface.peekSomeMaybe

peekCharMaybe :: forall c m mode. Chunk c => Walk mode c m (Maybe (One c))
peekCharMaybe = Walk Interface.peekCharMaybe

atEnd :: forall c m mode. Walk mode c m Bool
atEnd = Walk Interface.atEnd

takeText :: forall c m. Chunk c => ChunkCharacterEquivalence c -> c -> Walk 'RW c m Bool
takeText eq = \t -> Walk (go t)
  where
    go :: c -> Factory (Step 'RW c) m Bool
    go t = Interface.peekSomeMaybe >>= \case
        Nothing -> pure False
        Just x -> case stripEitherPrefix eq x t of
            StripEitherPrefixFail           ->  pure False
            StripEitherPrefixAll            ->  Interface.commit (length t) <&> \_ -> True
            IsPrefixedBy{}                  ->  Interface.commit (length t) <&> \_ -> True
            IsPrefixOf{ afterPrefix = t' }  ->  Interface.commit (length x) *> go t'

nextTextIs :: forall mode c m. Chunk c => ChunkCharacterEquivalence c -> c -> Walk mode c m Bool
nextTextIs eq = \t -> Walk (go t)
  where
    go :: c -> Factory (Step mode c) m Bool
    go t = Interface.peekSomeMaybe >>= \case
        Nothing -> pure False
        Just x -> case stripEitherPrefix eq x t of
            StripEitherPrefixFail           ->  pure False
            StripEitherPrefixAll            ->  pure True
            IsPrefixedBy{}                  ->  pure True
            IsPrefixOf{ afterPrefix = t' }  ->  go t'
