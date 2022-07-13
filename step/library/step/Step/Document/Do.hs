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

import Step.Document.Parser (Parser)

import Step.ActionTypes (Join, type (>>), SureQuery)
import qualified Step.ActionTypes as Action

join :: (Join k1 k2, Monad m) =>
    Parser text m k1 (Parser text m k2 a)
    -> Parser text m (k1 >> k2) a
join = Action.joinT

infixl 1 >>=
(>>=) ::
    Monad m => Join k1 k2 => k1 >> k2 ~ k3 =>
    Parser text m k1 a -> (a -> Parser text m k2 b) -> Parser text m k3 b
x >>= f = join (fmap f x)

infixl 4 <*
(<*) ::
    Monad m => Join k1 k2 => k1 >> k2 ~ k3 =>
    Parser text m k1 a -> Parser text m k2 b -> Parser text m k3 a
a <* b = join (fmap (\x -> fmap (\_ -> x) b) a)

infixl 4 *>
(*>) ::
    Monad m => Join k1 k2 => k1 >> k2 ~ k3 =>
    Parser text m k1 a -> Parser text m k2 b -> Parser text m k3 b
a *> b = join (fmap (\_ -> b) a)

infixl 1 >>
(>>) = (*>)

infixl 4 <*>
(<*>) ::
    Monad m => Join k1 k2 => k1 >> k2 ~ k3 =>
    Parser text m k1 (a -> b) -> Parser text m k2 a -> Parser text m k3 b
f <*> x = join (fmap (\f' -> fmap f' x) f)

return :: Monad m => a -> Parser text m SureQuery a
return = BasePrelude.return

pure = return
