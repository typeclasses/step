module Step.Action.Lift where

import Step.Internal.Prelude hiding (lift)

import Step.Action.Family (Action)

import qualified Step.Action.Kind as Kind
import Step.Action.Kind (ActionKind)

class Lift (k1 :: ActionKind) (k2 :: ActionKind) where
    lift :: Monad m =>
        Action k1 config cursor error m a
        -> Action k2 config cursor error m a

instance Lift 'Kind.Any 'Kind.Any where

instance Lift 'Kind.Move 'Kind.Any where

instance Lift 'Kind.MoveUndo 'Kind.Any where

instance Lift 'Kind.MoveUndo 'Kind.Move where

instance Lift 'Kind.Sure 'Kind.Any where

instance Lift 'Kind.SureStatic 'Kind.Any where

instance Lift 'Kind.SureStatic 'Kind.Sure where

liftTo :: forall k2 k1 config cursor error m a.
    Monad m =>
    Lift k1 k2 =>
    Action k1 config cursor error m a
    -> Action k2 config cursor error m a
liftTo = lift @k1 @k2
