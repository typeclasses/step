{-# options_ghc -fno-warn-missing-signatures #-}

module Variado.Monad.Do
  (
    {- * Monad -} join, (>>=), (>>),
    {- * Applicative -} (<*), (*>), (<*>),
    {- * Functor -} fmap, (<$>),
    {- * Extras -} replicateM_,
  )
  where

import Natural (Natural)
import qualified BasePrelude
import BasePrelude (fmap, (<$>), Functor, (-), Applicative)
import Kind (Type)
import Functor (void)

import Variado.Monad.Class

(>>=) :: PolyMonad m1 m2 => Functor m1 => Functor m2 => m1 a -> (a -> m2 b) -> (Join m1 m2) b
x >>= f = join (fmap f x)

(<*) :: PolyMonad m1 m2 => Functor m1 => Functor m2 => m1 a -> m2 b -> (Join m1 m2) a
a <* b = join (fmap (\x -> fmap (\_ -> x) b) a)

(*>) :: PolyMonad m1 m2 => Functor m1 => Functor m2 => m1 a -> m2 b -> (Join m1 m2) b
a *> b = join (fmap (\_ -> b) a)

(>>) = (*>)

(<*>) :: PolyMonad m1 m2 => Functor m1 => Functor m2 => m1 (a -> b) -> m2 a -> (Join m1 m2) b
f <*> x = join (fmap (\f' -> fmap f' x) f)

replicateM_ :: Applicative (Join m m) => Join m (Join m m) ~ Join m m => PolyMonad m m => PolyMonad m (Join m m) => Functor m => Natural -> m a -> (Join m m) ()
replicateM_ = \case
    0 -> \_ -> BasePrelude.pure ()
    n -> \a -> a *> replicateM_ (n - 1) a
