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