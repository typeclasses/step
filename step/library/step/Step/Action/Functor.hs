module Step.Action.Functor where

import Step.Internal.Prelude

import Step.Action.Kinds

import qualified Monad

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

class FunctorAction action => MonadAction (action :: ActionKind) where
    pureAction :: Monad m => a -> action config cursor error m a
    bindAction :: Monad m => action config cursor error m a -> (a -> action config cursor error m b) -> action config cursor error m b

instance MonadAction Any        where pureAction = pure ; bindAction = (Monad.>>=)
instance MonadAction Static     where pureAction = pure ; bindAction = (Monad.>>=)
instance MonadAction Move       where pureAction = pure ; bindAction = (Monad.>>=)
instance MonadAction Sure       where pureAction = pure ; bindAction = (Monad.>>=)
instance MonadAction SureStatic where pureAction = pure ; bindAction = (Monad.>>=)
instance MonadAction SureMove   where pureAction = pure ; bindAction = (Monad.>>=)
