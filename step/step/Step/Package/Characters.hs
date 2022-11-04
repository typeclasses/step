module Step.Package.Characters
  (
    peekChar, takeChar, satisfyJust, satisfyPredicate,
    nextCharIs, takeParticularChar, trySkipChar,
  )
  where

import Step.Action.Core
import Step.Chunk
import Step.Package.FixedLength
import Step.Package.Failure
import Step.Interface

import qualified Step.Do as P
import qualified Step.Interface as Interface

import Control.Monad ((>>=))
import Data.Bool (Bool (..))
import Data.Either (Either (..))
import Data.Eq (Eq, (==))
import Data.Function (($))
import Data.Functor (($>), (<&>), fmap)
import Data.Maybe (Maybe (..))
import Numeric.Natural (Natural)
import NatOptics.Positive.Unsafe (Positive (PositiveUnsafe))
import SupplyChain (perform, order)

import qualified SupplyChain

-- | Take a peek at the next character (if possible) without advancing
peekCharMaybe :: forall c m r. Chunk c => SureQuery c m r r (Maybe (One c))
peekCharMaybe = SureQuery \_ -> ResettingSequenceJob $ order nextMaybe <&> fmap @Maybe head

peekChar :: forall c m r. Chunk c => Query c m r r (One c)
peekChar = peekCharMaybe P.>>= requireJust

-- | Advance over the next character and return it; fail if end of input
takeChar :: forall c m r. Chunk c => AtomicMove c m r r (One c)
takeChar = assumeMovement $ peekCharMaybe P.>>= requireJust P.>>= (trySkipChar $>)

nextCharIs :: forall c m r. Chunk c => Eq (One c) => One c -> SureQuery c m r r Bool
nextCharIs c = act \_ -> order nextMaybe <&> \case{ Just x | head x == c -> True; _ -> False }

takeParticularChar :: forall c m r. Chunk c => Eq (One c) => One c -> AtomicMove c m r r ()
takeParticularChar c = assumeMovement P.do{ nextCharIs c P.>>= requireTrue; trySkipChar }

satisfyJust :: forall c m r a. Chunk c => (One c -> Maybe a) -> AtomicMove c m r r a
satisfyJust ok = assumeMovement P.do{ x <- peekChar; y <- requireJust (ok x); trySkipChar; P.pure y }

satisfyPredicate :: forall c m r. Chunk c => (One c -> Bool) -> AtomicMove c m r r (One c)
satisfyPredicate ok = assumeMovement P.do{ x <- peekChar; requireTrue (ok x); trySkipChar; P.pure x }

trySkipChar :: Sure c m r r ()
trySkipChar = trySkipPositive_ (PositiveUnsafe 1)
