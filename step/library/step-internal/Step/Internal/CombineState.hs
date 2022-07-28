{-# language Trustworthy #-}

module Step.Internal.CombineState where

import Optics
import State

combineState :: Monad m => Iso' (s1, s2) s -> StateT s2 (StateT s1 m) a -> StateT s m a
combineState o a = StateT \s -> do
    let b = runStateT a (view (re o % _2) s)
    ((x, s2), s1) <- runStateT b (view (re o % _1) s)
    return (x, view o (s1, s2))

splitState :: Monad m => Iso' (s1, s2) s -> StateT s m a -> StateT s2 (StateT s1 m) a
splitState o a = do

    s1 <- lift get
    s2 <- get

    let s = view o (s1, s2)
    (x, s') <- lift (lift (runStateT a s))
    let (s1', s2') = review o s'

    lift (put s1')
    put s2'

    return x

twoStateIso :: Monad m => Iso' (s1, s2) s
    -> Iso (StateT s2 (StateT s1 m) a) (StateT s2 (StateT s1 m) b) (StateT s m a) (StateT s m b)
twoStateIso o = iso (combineState o) (splitState o)
