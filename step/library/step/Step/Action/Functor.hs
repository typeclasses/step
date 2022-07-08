{-# language Safe #-}

module Step.Action.Functor where

import Step.Internal.Prelude

import Step.Action.Types

class
  ( forall config cursor error m. Functor m =>
      Functor (k config cursor error m)
  ) =>
      FunctorAction k

instance FunctorAction Any
instance FunctorAction Query
instance FunctorAction Move
instance FunctorAction Atom
instance FunctorAction AtomicMove
instance FunctorAction Sure
instance FunctorAction SureQuery

class (FunctorAction action, forall config cursor error m. Monad m =>
      Monad (action config cursor error m)) => MonadAction (action :: ActionKind)

instance MonadAction Any
instance MonadAction Query
instance MonadAction Sure
instance MonadAction SureQuery
