{-# language DataKinds, FunctionalDependencies, KindSignatures, MultiParamTypeClasses, Unsafe #-}

module Step.ActionTypes.Coerce where

import Optics hiding (coerced)
import qualified Optics

import Step.ActionTypes.Constructors

class Coerce (act1 :: Action) (act2 :: Action) | act2 -> act1 where
    coerced :: Iso (act1 xs1 x1 r1 s1 m1 a1) (act1 xs2 x2 r2 s2 m2 a2) (act2 xs1 x1 r1 s1 m1 a1) (act2 xs2 x2 r2 s2 m2 a2)

to :: forall act2 act1 xs x r s m a. Coerce act2 act1 => act1 xs x r s m a -> act2 xs x r s m a
to = review coerced

from :: forall act1 act2 xs x r s m a. Coerce act1 act2 => act1 xs x r s m a -> act2 xs x r s m a
from = view coerced

instance Coerce Any Any where coerced = Optics.coerced
instance Coerce Any Query where coerced = Optics.coerced
instance Coerce Any Move where coerced = Optics.coerced
instance Coerce Any Atom where coerced = Optics.coerced
instance Coerce Any AtomicMove where coerced = Optics.coerced

instance Coerce Sure Sure where coerced = Optics.coerced
instance Coerce Sure SureQuery where coerced = Optics.coerced

instance Coerce Fail Fail where coerced = Optics.coerced
