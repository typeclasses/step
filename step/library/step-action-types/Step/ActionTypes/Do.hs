{-# options_ghc -fno-warn-missing-signatures #-}
{-# language FlexibleContexts, Safe, TypeFamilies, TypeOperators #-}

-- | This module is for use with the QualifiedDo language extension.

module Step.ActionTypes.Do
  (
    {- * Monad -} join, (>>=), (>>), return,
    {- * Applicative -} (<*), (*>), (<*>), pure,
    {- * Functor -} fmap, (<$>),
  )
  where

import qualified BasePrelude
import BasePrelude (fmap, (<$>), Monad, (.))
import Function ((&))

import Step.ActionTypes.Join (Join, join)
import Step.ActionTypes.KindJoin (type (>>))
import Step.ActionTypes.Types (SureQuery)

infixl 1 >>=
(>>=) :: Monad m => Join act1 act2 => act1 >> act2 ~ act3 => act1 m e a -> (a -> act2 m e b) -> act3 m e b
x >>= f = join (fmap f x)

infixl 4 <*
(<*) :: Monad m => Join act1 act2 => act1 >> act2 ~ act3 => act1 m e a -> act2 m e b -> act3 m e a
a <* b = join (fmap (\x -> fmap (\_ -> x) b) a)

infixl 4 *>
(*>) :: Monad m => Join act1 act2 => act1 >> act2 ~ act3 => act1 m e a -> act2 m e b -> act3 m e b
a *> b = join (fmap (\_ -> b) a)

infixl 1 >>
(>>) = (*>)

infixl 4 <*>
(<*>) :: Monad m => Join act1 act2 => act1 >> act2 ~ act3 => act1 m e (a -> b) -> act2 m e a -> act3 m e b
f <*> x = join (fmap (\f' -> fmap f' x) f)

return :: Monad m => a -> SureQuery m e a
return = BasePrelude.return

pure = return
