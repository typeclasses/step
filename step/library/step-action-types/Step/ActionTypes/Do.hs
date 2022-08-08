{-# options_ghc -fno-warn-missing-signatures #-}
{-# language FlexibleContexts, TypeOperators #-}

-- | This module is for use with the QualifiedDo language extension.

module Step.ActionTypes.Do
  (
    {- * Monad -} join, (>>=), (>=>), (>>), return,
    {- * Applicative -} (<*), (*>), (<*>), pure,
    {- * Functor -} fmap, (<$>), (<&>),
  )
  where

import qualified BasePrelude
import BasePrelude (fmap, (<$>), Monad)

import Step.ActionTypes.Join (Join, join)
import Step.ActionTypes.KindJoin (type (>>))
import Step.ActionTypes.Constructors

infixl 1 >>=
(>>=) :: Monad m => Join act1 act2 => act1 >> act2 ~ act3 => act1 xs x r s m a -> (a -> act2 xs x r s m b) -> act3 xs x r s m b
x >>= f = join (fmap f x)

infixr 1 >=>
(>=>) :: Monad m => Join act1 act2 => act1 >> act2 ~ act3 => (a -> act1 xs x r s m b) -> (b -> act2 xs x r s m c) -> a -> act3 xs x r s m c
a >=> b = \x -> a x >>= b

infixl 4 <*
(<*) :: Monad m => Join act1 act2 => act1 >> act2 ~ act3 => act1 xs x r s m a -> act2 xs x r s m b -> act3 xs x r s m a
a <* b = join (fmap (\x -> fmap (\_ -> x) b) a)

infixl 4 *>
(*>) :: Monad m => Join act1 act2 => act1 >> act2 ~ act3 => act1 xs x r s m a -> act2 xs x r s m b -> act3 xs x r s m b
a *> b = join (fmap (\_ -> b) a)

infixl 1 >>
(>>) = (*>)

infixl 4 <*>
(<*>) :: Monad m => Join act1 act2 => act1 >> act2 ~ act3 => act1 xs x r s m (a -> b) -> act2 xs x r s m a -> act3 xs x r s m b
f <*> x = join (fmap (\f' -> fmap f' x) f)

return :: Monad m => a -> SureQuery xs x r s m a
return = BasePrelude.return

pure = return

x <&> f = f <$> x
