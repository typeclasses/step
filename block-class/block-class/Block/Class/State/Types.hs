module Block.Class.State.Types
  (
    {- * Types -} State (..), StateResult (..),
  )
  where

import Essentials

newtype State s a = State (s -> (StateResult s a))
    deriving stock Functor

data StateResult s a = StateResult{ stateResult :: a, newState :: s }
    deriving stock (Eq, Ord, Show, Functor)

instance Applicative (State s) where
    pure x = State \s -> StateResult{ stateResult = x, newState = s }
    State sf <*> State sx = State \s ->
      let
        StateResult{ stateResult = f, newState = s1 } = sf s
        StateResult{ stateResult = x, newState = s2 } = sx s1
      in
        StateResult{ stateResult = f x, newState = s2 }

instance Monad (State s) where
    State sa >>= f = State \s ->
      let
        StateResult{ stateResult = a, newState = s1 } = sa s
        State sb = f a
      in
        sb s1
