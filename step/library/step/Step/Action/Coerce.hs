module Step.Action.Coerce where

import Optics hiding (coerced)
import qualified Optics

import Step.Action.Kinds

class Coerce k1 k2
  where
    coerced :: Iso
        (k1 config1 cursor1 error1 m1 a1)
        (k1 config2 cursor2 error2 m2 a2)
        (k2 config1 cursor1 error1 m1 a1)
        (k2 config2 cursor2 error2 m2 a2)

to :: forall k2 k1 config cursor error m a. Coerce k2 k1 =>
    k1 config cursor error m a -> k2 config cursor error m a
to = review coerced

from :: forall k1 k2 config cursor error m a. Coerce k1 k2 =>
    k1 config cursor error m a -> k2 config cursor error m a
from = view coerced

instance Coerce Any Any        where coerced = Optics.coerced
instance Coerce Any Query      where coerced = Optics.coerced
instance Coerce Any Move       where coerced = Optics.coerced
instance Coerce Any Atom       where coerced = Optics.coerced
instance Coerce Any MoveAtom   where coerced = Optics.coerced

instance Coerce Sure Sure      where coerced = Optics.coerced
instance Coerce Sure SureQuery where coerced = Optics.coerced
instance Coerce Sure SureMove  where coerced = Optics.coerced
