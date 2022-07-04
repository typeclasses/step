{-# options_ghc -fno-warn-missing-signatures #-}

module Step.Document.Do
  (
    {- * Monad -} join, (>>=), (>>), return,
    {- * Applicative -} (<*), (*>), (<*>), pure,
    {- * Functor -} fmap, (<$>),
    {- * Extra -} replicateM_,
  )
  where

import qualified BasePrelude
import Natural (Natural)
import BasePrelude (fmap, (<$>), Functor, Monad)
import Kind (Type)

import Variado.Monad.Class
import qualified Variado.Monad.Do as Variado

import Step.Document.Parser (Parser, action')
import Step.Action.UnifiedType (ActionJoin, IsAction, (:>))
import Step.Action.SeparateTypes (SureStatic (SureStatic), MonadAction)

import qualified Optics as O

(>>=) ::
    Monad m => IsAction k1 => IsAction k2 => IsAction k3 => ActionJoin k1 k2 => k1 :> k2 ~ k3 =>
    Parser text k1 m a -> (a -> Parser text k2 m b) -> Parser text k3 m b
(>>=) = (Variado.>>=)

(<*) ::
    Monad m => IsAction k1 => IsAction k2 => IsAction k3 => ActionJoin k1 k2 => k1 :> k2 ~ k3 =>
    Parser text k1 m a -> Parser text k2 m b -> Parser text k3 m a
(<*) = (Variado.<*)

(*>) ::
    Monad m => IsAction k1 => IsAction k2 => IsAction k3 => ActionJoin k1 k2 => k1 :> k2 ~ k3 =>
    Parser text k1 m a -> Parser text k2 m b -> Parser text k3 m b
(*>) = (Variado.*>)

(>>) = (*>)

(<*>) ::
    Monad m => IsAction k1 => IsAction k2 => IsAction k3 => ActionJoin k1 k2 => k1 :> k2 ~ k3 =>
    Parser text k1 m (a -> b) -> Parser text k2 m a -> Parser text k3 m b
(<*>) = (Variado.<*>)

return :: Monad m => a -> Parser text SureStatic m a
return x = O.review action' (SureStatic \_config s -> BasePrelude.return (x, s))

pure = return

replicateM_ ::
    Monad m => IsAction k1 => IsAction k2 => ActionJoin k1 k1 => ActionJoin k1 k2 =>
    MonadAction k2 => k1 :> k1 ~ k2 => k1 :> k2 ~ k2 =>
    Natural -> Parser text k1 m a -> Parser text k2 m ()
replicateM_ = Variado.replicateM_
