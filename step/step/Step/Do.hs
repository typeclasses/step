{- |

Description: For use with the QualifiedDo language extension

-}

module Step.Do
  (
    {- * Monad -} join, (>>=), (>=>), (>>), return,
    {- * Applicative -} (<*), (*>), (<*>), pure,
    {- * Functor -} fmap, (<$>), (<&>),
  )
  where

import qualified BasePrelude
import BasePrelude (fmap, (<$>), Monad)

import Step.Action

infixl 1 >>=
(>>=) :: Monad m => Join act1 act2 => act1 >> act2 ~ act3 => act1 xs x e m a -> (a -> act2 xs x e m b) -> act3 xs x e m b
(>>=) = bindAction

infixr 1 >=>
(>=>) :: Monad m => Join act1 act2 => act1 >> act2 ~ act3 => (a -> act1 xs x e m b) -> (b -> act2 xs x e m c) -> a -> act3 xs x e m c
a >=> b = \x -> a x >>= b

infixl 4 <*
(<*) :: Monad m => Join act1 act2 => act1 >> act2 ~ act3 => act1 xs x e m a -> act2 xs x e m b -> act3 xs x e m a
a <* b = join (fmap (\x -> fmap (\_ -> x) b) a)

infixl 4 *>
(*>) :: Monad m => Join act1 act2 => act1 >> act2 ~ act3 => act1 xs x e m a -> act2 xs x e m b -> act3 xs x e m b
a *> b = join (fmap (\_ -> b) a)

infixl 1 >>
(>>) = (*>)

infixl 4 <*>
(<*>) :: Monad m => Join act1 act2 => act1 >> act2 ~ act3 => act1 xs x e m (a -> b) -> act2 xs x e m a -> act3 xs x e m b
f <*> x = join (fmap (\f' -> fmap f' x) f)

return :: Monad m => a -> SureQuery xs x e m a
return = BasePrelude.return

pure = return

x <&> f = f <$> x
