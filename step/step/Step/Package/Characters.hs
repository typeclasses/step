module Step.Package.Characters
  (
    peekChar, takeChar, satisfyJust, satisfyPredicate,
    nextCharIs, takeParticularChar, trySkipChar,
  )
  where

import Essentials
import Step.Action.Core
import Block.Class
import Step.Package.FixedLength
import Step.Package.Failure
import Step.Interface
import Step.LeftRight

import qualified Step.Do as P
import qualified Integer.Positive as Positive

import SupplyChain (order)


-- | Take a peek at the next character (if possible) without advancing
peekCharMaybe :: forall c m r. Block c => SureQuery c m r (Step (One c))
peekCharMaybe = SureQuery \_ -> ResettingSequenceJob $ order next <&> fmap @Step head

peekChar :: forall c m r. Block c => Query c m r (One c)
peekChar = peekCharMaybe P.>>= requireItem

-- | Advance over the next character and return it; fail if end of input
takeChar :: forall c m r. Block c => Atom c m r (One c)
takeChar = peekCharMaybe P.>>= requireItem P.>>= (trySkipChar $>)

nextCharIs :: forall c m r. Block c => Eq (One c) => One c -> SureQuery c m r Bool
nextCharIs c = act \_ -> order next <&> right . \case{ Item x | head x == c -> True; _ -> False }

takeParticularChar :: forall c m r. Block c => Eq (One c) => One c -> Atom c m r ()
takeParticularChar c = P.do{ nextCharIs c P.>>= requireTrue; trySkipChar }

satisfyJust :: forall c m r a. Block c => (One c -> Maybe a) -> Atom c m r a
satisfyJust ok = P.do{ x <- peekChar; y <- requireJust (ok x); trySkipChar; P.pure y }

satisfyPredicate :: forall c m r. Block c => (One c -> Bool) -> Atom c m r (One c)
satisfyPredicate ok = P.do{ x <- peekChar; requireTrue (ok x); trySkipChar; P.pure x }

trySkipChar :: Sure c m r ()
trySkipChar = trySkipPositive_ Positive.one
