module Step.Action.Atomic where

import Step.Internal.Prelude

import Step.Action.Kinds

import qualified Step.Action.Coerce as Coerce

class Atomic (k :: ActionKind)
  where
    type Try k :: ActionKind
    try :: Functor m => k config cursor error m a -> (Try k) config cursor error m (Maybe a)

instance Atomic Undo
  where
    type Try Undo = Sure
    try = Coerce.from @Sure . tryAnySure . Coerce.to @Any

instance Atomic MoveUndo
  where
    type Try MoveUndo = SureMove
    try = Coerce.from @Sure . tryAnySure . Coerce.to @Any

instance Atomic Static
  where
    type Try Static = SureStatic
    try = Coerce.from @Sure . tryAnySure . Coerce.to @Any

tryAnySure :: Functor m => Any config cursor error m a -> Sure config cursor error m (Maybe a)
tryAnySure (Any p) = Sure \c -> p c <&> \case
    Left _ -> Nothing
    Right x -> Just x
