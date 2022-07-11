{-# options_ghc -fno-warn-missing-signatures #-}
{-# language FlexibleContexts, Trustworthy, TypeFamilies, TypeOperators #-}

module Step.Document.Do
  (
    {- * Monad -} join, (>>=), (>>), return,
    {- * Applicative -} (<*), (*>), (<*>), pure,
    {- * Functor -} fmap, (<$>),
  )
  where

import qualified BasePrelude
import BasePrelude (fmap, (<$>), Monad, (.))
import Function ((&))

import Step.Document.Parser (Parser (Parser))

import Step.ActionTypes (Join, type (>>), SureQuery)
import qualified Step.ActionTypes as Action

join :: (Join k1 k2, Monad m) =>
    Parser text k1 m (Parser text k2 m a)
    -> Parser text (k1 >> k2) m a
join p = Parser \c ->
    p & fmap (\(Parser a) -> a c)
      & (\(Parser a) -> a c)
      & Action.join

infixl 1 >>=
(>>=) ::
    Monad m => Join k1 k2 => k1 >> k2 ~ k3 =>
    Parser text k1 m a -> (a -> Parser text k2 m b) -> Parser text k3 m b
x >>= f = join (fmap f x)

infixl 4 <*
(<*) ::
    Monad m => Join k1 k2 => k1 >> k2 ~ k3 =>
    Parser text k1 m a -> Parser text k2 m b -> Parser text k3 m a
a <* b = join (fmap (\x -> fmap (\_ -> x) b) a)

infixl 4 *>
(*>) ::
    Monad m => Join k1 k2 => k1 >> k2 ~ k3 =>
    Parser text k1 m a -> Parser text k2 m b -> Parser text k3 m b
a *> b = join (fmap (\_ -> b) a)

infixl 1 >>
(>>) = (*>)

infixl 4 <*>
(<*>) ::
    Monad m => Join k1 k2 => k1 >> k2 ~ k3 =>
    Parser text k1 m (a -> b) -> Parser text k2 m a -> Parser text k3 m b
f <*> x = join (fmap (\f' -> fmap f' x) f)

return :: Monad m => a -> Parser text SureQuery m a
return x = Parser (\_ -> BasePrelude.return x)

pure = return
