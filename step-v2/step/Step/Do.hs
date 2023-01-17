{- |

Description: For use with the QualifiedDo language extension

This module is intended for use with the
<https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/qualified_do.html QualifiedDo>
language extension.

-}

module Step.Do
  (
    {- * Monad -} join, (>>=), (>=>), (>>), return,
    {- * Applicative -} (<*), (*>), (<*>), pure, pure',
    {- * Functor -} fmap, (<$>), (<&>),
  )
  where

import Data.Functor (Functor)

import qualified Control.Applicative as Applicative
import qualified Data.Functor as Functor

import Step.Action.Core (Join, type (>>), SureQuery, pure')
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
    act1 c m r (act2 c m r a) -> (act1 >> act2) c m r a
join = A.join

-- | Akin to 'Monad.>>='
(>>=) :: Functor (act1 c m r) => Join act1 act2 => act1 >> act2 ~ act3 =>
    act1 c m r a -> (a -> act2 c m r b) -> act3 c m r b
(>>=) = A.bindAction

-- | Akin to 'Monad.>=>'
(>=>) :: Functor (act1 c m r) => Join act1 act2 => act1 >> act2 ~ act3 =>
    (a -> act1 c m r b) -> (b -> act2 c m r c) -> a -> act3 c m r c
a >=> b = \x -> a x >>= b

-- | Akin to 'Applicative.<*'
(<*) :: Functor (act1 c m r) => Functor (act2 c m r) => Join act1 act2 =>
    act1 >> act2 ~ act3 => act1 c m r a -> act2 c m r b -> act3 c m r a
a <* b = join (fmap (\x -> fmap (\_ -> x) b) a)

-- | Akin to 'Applicative.*>'
(*>) :: Functor (act1 c m r) => Join act1 act2 => act1 >> act2 ~ act3 =>
    act1 c m r a -> act2 c m r b -> act3 c m r b
a *> b = join (fmap (\_ -> b) a)

-- | Akin to 'Monad.>>'
(>>) :: Functor (act1 c m r) => Join act1 act2 => act1 >> act2 ~ act3 =>
    act1 c m r a -> act2 c m r b -> act3 c m r b
(>>) = (*>)

-- | Akin to 'Applicative.<*>'
(<*>) :: Functor (act1 c m r) => Functor (act2 c m r) =>
    Join act1 act2 => act1 >> act2 ~ act3 =>
    act1 c m r (a -> b) -> act2 c m r a -> act3 c m r b
f <*> x = join (fmap (\f' -> fmap f' x) f)

-- | Specialized alias for 'Applicative.pure'
return :: a -> SureQuery c m r a
return = Applicative.pure

-- | Specialized alias for 'Applicative.pure'
pure :: a -> SureQuery c m r a
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
