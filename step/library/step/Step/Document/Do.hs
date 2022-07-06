{-# options_ghc -fno-warn-missing-signatures #-}

module Step.Document.Do
  (
    {- * Monad -} join, (>>=), (>>), return,
    {- * Applicative -} (<*), (*>), (<*>), pure,
    {- * Functor -} fmap, (<$>),
  )
  where

import qualified BasePrelude
import BasePrelude (fmap, (<$>), Monad, (.))

import Step.Document.Parser (Parser (Parser))

import Step.Action.Safe

join :: (ActionJoin k1 k2, Monad m,
    FunctorAction k1) =>
    Parser text k1 m (Parser text k2 m a)
    -> Parser text (k1 :> k2) m a
join = Parser . actionJoin . (\(Parser a) -> a) . fmap (\(Parser a) -> a)

(>>=) ::
    Monad m => IsAction k1 => IsAction k2 => IsAction k3 => ActionJoin k1 k2 => k1 :> k2 ~ k3 =>
    Parser text k1 m a -> (a -> Parser text k2 m b) -> Parser text k3 m b
x >>= f = join (fmap f x)

(<*) ::
    Monad m => IsAction k1 => IsAction k2 => IsAction k3 => ActionJoin k1 k2 => k1 :> k2 ~ k3 =>
    Parser text k1 m a -> Parser text k2 m b -> Parser text k3 m a
a <* b = join (fmap (\x -> fmap (\_ -> x) b) a)

(*>) ::
    Monad m => IsAction k1 => IsAction k2 => IsAction k3 => ActionJoin k1 k2 => k1 :> k2 ~ k3 =>
    Parser text k1 m a -> Parser text k2 m b -> Parser text k3 m b
a *> b = join (fmap (\_ -> b) a)

(>>) = (*>)

(<*>) ::
    Monad m => IsAction k1 => IsAction k2 => IsAction k3 => ActionJoin k1 k2 => k1 :> k2 ~ k3 =>
    Parser text k1 m (a -> b) -> Parser text k2 m a -> Parser text k3 m b
f <*> x = join (fmap (\f' -> fmap f' x) f)

return :: Monad m => a -> Parser text SureQuery m a
return x = Parser (BasePrelude.return x)

pure = return
