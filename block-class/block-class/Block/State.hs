module Block.State
  (
    {- * Type -} State (..), StateResult (..),
    {- * Utilities -} runState, evalState, execState, stateless, get, put, modify,
  )
  where

import Essentials

import Prelude (($!))

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

stateless :: State () a -> a
stateless = evalState ()

runState :: s -> State s a -> StateResult s a
runState s (State f) = f s

evalState :: s -> State s a -> a
evalState s (State f) = stateResult (f s)

execState :: s -> State s a -> s
execState s (State f) = newState (f s)

get :: State s s
get = State \s -> StateResult s s

put :: s -> State s ()
put s = State \_ -> StateResult () s

modify :: (s -> s) -> State s ()
modify f = do
    s <- get
    put $! f s
