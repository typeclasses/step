module Step.Action.Functor where

import Step.Internal.Prelude

import Step.Action.Kinds

class
  ( forall config cursor error m. Functor m =>
      Functor (k config cursor error m)
  ) =>
      FunctorAction k

instance FunctorAction Any
instance FunctorAction Static
instance FunctorAction Move
instance FunctorAction Undo
instance FunctorAction MoveUndo
instance FunctorAction Sure
instance FunctorAction SureStatic
instance FunctorAction SureMove

class (FunctorAction action, forall config cursor error m. Monad m =>
      Monad (action config cursor error m)) => MonadAction (action :: ActionKind)

instance MonadAction Any
instance MonadAction Static
instance MonadAction Sure
instance MonadAction SureStatic
