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

import qualified Prelude
import Prelude (Functor, fmap, (<$>))

import Step.Action.Core

infixl 1 >>=
(>>=) :: Functor (act1 c m e) => Join act1 act2 => act1 >> act2 ~ act3 =>
    act1 c m e a -> (a -> act2 c m e b) -> act3 c m e b
(>>=) = bindAction

infixr 1 >=>
(>=>) :: Functor (act1 c m e) => Join act1 act2 => act1 >> act2 ~ act3 =>
    (a -> act1 c m e b) -> (b -> act2 c m e c) -> a -> act3 c m e c
a >=> b = \x -> a x >>= b

infixl 4 <*
(<*) :: Functor (act1 c m e) => Functor (act2 c m e) => Join act1 act2 =>
    act1 >> act2 ~ act3 => act1 c m e a -> act2 c m e b -> act3 c m e a
a <* b = join (fmap (\x -> fmap (\_ -> x) b) a)

infixl 4 *>
(*>) :: Functor (act1 c m e) => Join act1 act2 => act1 >> act2 ~ act3 =>
    act1 c m e a -> act2 c m e b -> act3 c m e b
a *> b = join (fmap (\_ -> b) a)

infixl 1 >>
(>>) :: Functor (act1 c m e) => Join act1 act2 => act1 >> act2 ~ act3 =>
    act1 c m e a -> act2 c m e b -> act3 c m e b
(>>) = (*>)

infixl 4 <*>
(<*>) :: Functor (act1 c m e) => Functor (act2 c m e) => Join act1 act2 => act1 >> act2 ~ act3 =>
    act1 c m e (a -> b) -> act2 c m e a -> act3 c m e b
f <*> x = join (fmap (\f' -> fmap f' x) f)

return :: a -> SureQuery c m e a
return = Prelude.return

pure :: a -> SureQuery c m e a
pure = return

(<&>) :: Functor f => f a -> (a -> b) -> f b
x <&> f = f <$> x
