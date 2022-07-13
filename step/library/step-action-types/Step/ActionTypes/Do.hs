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

import Step.ActionTypes (Join, type (>>), SureQuery)
import qualified Step.ActionTypes as Action

join :: (Join k1 k2, Monad m) => k1 e m (k2 e m a) -> (k1 >> k2) e m a
join = Action.join

infixl 1 >>=
(>>=) :: Monad m => Join k1 k2 => k1 >> k2 ~ k3 => k1 e m a -> (a -> k2 e m b) -> k3 e m b
x >>= f = join (fmap f x)

infixl 4 <*
(<*) :: Monad m => Join k1 k2 => k1 >> k2 ~ k3 => k1 e m a -> k2 e m b -> k3 e m a
a <* b = join (fmap (\x -> fmap (\_ -> x) b) a)

infixl 4 *>
(*>) :: Monad m => Join k1 k2 => k1 >> k2 ~ k3 => k1 e m a -> k2 e m b -> k3 e m b
a *> b = join (fmap (\_ -> b) a)

infixl 1 >>
(>>) = (*>)

infixl 4 <*>
(<*>) :: Monad m => Join k1 k2 => k1 >> k2 ~ k3 => k1 e m (a -> b) -> k2 e m a -> k3 e m b
f <*> x = join (fmap (\f' -> fmap f' x) f)

return :: Monad m => a -> SureQuery e m a
return = BasePrelude.return

pure = return
