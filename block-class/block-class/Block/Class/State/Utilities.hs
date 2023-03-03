module Block.Class.State.Utilities
  (
    {- * Utilities -}
        stateless, runState, evalState, execState,
        get, put, modify,
  )
  where

import Prelude (($!))

import Block.Class.State.Types

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
