module Block.Class.State.Types where

import Essentials

newtype State s a = State (s -> (s, a))
    deriving stock Functor

instance Applicative (State s) where
    pure x = State \s -> (s, x)
    State sf <*> State sx = State \s ->
      let
        (s1, f) = sf s
        (s2, x) = sx s1
      in
        (s2, f x)

instance Monad (State s) where
    State sa >>= f = State \s ->
      let
        (s1, a) = sa s
        State sb = f a
      in
        sb s1
