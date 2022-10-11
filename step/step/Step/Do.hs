{- |

Description: For use with the QualifiedDo language extension

This module is intended for use with the
<https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/qualified_do.html QualifiedDo>
language extension.

-}

module Step.Do
  (
    {- * Monad -} join, (>>=), (>=>), (>>), return,
    {- * Applicative -} (<*), (*>), (<*>), pure,
    {- * Functor -} fmap, (<$>), (<&>),
  )
  where

import Data.Functor (Functor)

import qualified Control.Applicative as Applicative
import qualified Data.Functor as Functor
import qualified Control.Monad as Monad

import Step.Action.Core (Join, type (>>), SureQuery)
import qualified Step.Action.Core as A

infixl 1 <&>
infixl 1 >>
infixl 1 >>=

infixl 4 *>
infixl 4 <*
infixl 4 <*>
infixl 4 <$>

infixr 1 >=>

-- | Akin to 'Monad.join'
join :: Join act1 act2 =>
    act1 c es e (act2 c es e a) -> (act1 >> act2) c es e a
join = A.join

-- | Akin to 'Monad.>>='
(>>=) :: Functor (act1 c m e) => Join act1 act2 => act1 >> act2 ~ act3 =>
    act1 c m e a -> (a -> act2 c m e b) -> act3 c m e b
(>>=) = A.bindAction

-- | Akin to 'Monad.>=>'
(>=>) :: Functor (act1 c m e) => Join act1 act2 => act1 >> act2 ~ act3 =>
    (a -> act1 c m e b) -> (b -> act2 c m e c) -> a -> act3 c m e c
a >=> b = \x -> a x >>= b

-- | Akin to 'Applicative.<*'
(<*) :: Functor (act1 c m e) => Functor (act2 c m e) => Join act1 act2 =>
    act1 >> act2 ~ act3 => act1 c m e a -> act2 c m e b -> act3 c m e a
a <* b = join (fmap (\x -> fmap (\_ -> x) b) a)

-- | Akin to 'Applicative.*>'
(*>) :: Functor (act1 c m e) => Join act1 act2 => act1 >> act2 ~ act3 =>
    act1 c m e a -> act2 c m e b -> act3 c m e b
a *> b = join (fmap (\_ -> b) a)

-- | Akin to 'Monad.>>'
(>>) :: Functor (act1 c m e) => Join act1 act2 => act1 >> act2 ~ act3 =>
    act1 c m e a -> act2 c m e b -> act3 c m e b
(>>) = (*>)

-- | Akin to 'Applicative.<*>'
(<*>) :: Functor (act1 c m e) => Functor (act2 c m e) =>
    Join act1 act2 => act1 >> act2 ~ act3 =>
    act1 c m e (a -> b) -> act2 c m e a -> act3 c m e b
f <*> x = join (fmap (\f' -> fmap f' x) f)

-- | Specialized alias for 'Applicative.pure'
return :: a -> SureQuery c m e a
return = Applicative.pure

-- | Specialized alias for 'Applicative.pure'
pure :: a -> SureQuery c m e a
pure = Applicative.pure

-- | Alias for 'Functor.fmap'
fmap :: Functor f => (a -> b) -> f a -> f b
fmap = Functor.fmap

-- | Alias for 'Functor.fmap'
(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = Functor.fmap

-- | Alias for 'Functor.<&>'
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = (Functor.<&>)
