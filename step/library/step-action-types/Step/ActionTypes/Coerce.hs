{-# language DataKinds, FunctionalDependencies, KindSignatures, MultiParamTypeClasses, Unsafe #-}

module Step.ActionTypes.Coerce where

import Optics hiding (coerced)
import qualified Optics

import Step.ActionTypes.Constructors

class Coerce (act1 :: Action) (act2 :: Action) | act2 -> act1 where
    coerced :: Iso (act1 m1 e1 a1) (act1 m2 e2 a2) (act2 m1 e1 a1) (act2 m2 e2 a2)

to :: forall act2 act1 m e a. Coerce act2 act1 => act1 m e a -> act2 m e a
to = review coerced

from :: forall act1 act2 m e a. Coerce act1 act2 => act1 m e a -> act2 m e a
from = view coerced

instance Coerce Any Any where coerced = Optics.coerced
instance Coerce Any Query where coerced = Optics.coerced
instance Coerce Any Move where coerced = Optics.coerced
instance Coerce Any Atom where coerced = Optics.coerced
instance Coerce Any AtomicMove where coerced = Optics.coerced

instance Coerce Sure Sure where coerced = Optics.coerced
instance Coerce Sure SureQuery where coerced = Optics.coerced

instance Coerce Fail Fail where coerced = Optics.coerced
