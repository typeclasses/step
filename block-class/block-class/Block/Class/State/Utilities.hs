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

runState :: s -> State s a -> (s, a)
runState s (State f) = f s

evalState :: s -> State s a -> a
evalState s (State f) = let (_, x) = f s in x

execState :: s -> State s a -> s
execState s (State f) = let (x, _) = f s in x

get :: State s s
get = State \s -> (s, s)

put :: s -> State s ()
put s = State \_ -> (s, ())

modify :: (s -> s) -> State s ()
modify f = do
    s <- get
    put $! f s
