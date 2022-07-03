module Step.Action.Do
  (
    join, (>>=),
    (<*), (*>), (>>), (<*>),
    fmap,
    pure, return
  )
  where

import Step.Action.Kind (ActionKind (..))

import qualified BasePrelude
import BasePrelude (Monad, Functor, Applicative, fmap)

import Step.Action.Join (Join)
import qualified Step.Action.Join as Join

import Step.Action.Family (Action)
import qualified Step.Action.Family as Action

join :: forall k1 k2 k3 config cursor error m a.
    Join k1 k2 k3 =>
    Action k1 config cursor error m (Action k2 config cursor error m a)
        -> Action k3 config cursor error m a
join = Join.join @k1 @k2 @k3

(>>=) :: forall k1 k2 k3 config cursor error m a b.
    Functor (Action k1 config cursor error m) =>
    Join k1 k2 k3 =>
    Action k1 config cursor error m a
    -> (a -> Action k2 config cursor error m b)
    -> Action k3 config cursor error m b
x >>= f = join (fmap f x)

(<*) :: forall k1 k2 k3 config cursor error m a b.
    Functor (Action k1 config cursor error m) =>
    Functor (Action k2 config cursor error m) =>
    Join k1 k2 k3 =>
    Action k1 config cursor error m a
    -> Action k2 config cursor error m b
    -> Action k3 config cursor error m a
a <* b = join @k1 @k2 @k3 (fmap (\x -> fmap (\_ -> x) b) a)

(*>) :: forall k1 k2 k3 config cursor error m a b.
    Functor (Action k1 config cursor error m) =>
    Join k1 k2 k3 =>
    Action k1 config cursor error m a
    -> Action k2 config cursor error m b
    -> Action k3 config cursor error m b
a *> b = join @k1 @k2 @k3 (fmap (\_ -> b) a)

(>>) :: forall k1 k2 k3 config cursor error m a b.
    Functor (Action k1 config cursor error m) =>
    Join k1 k2 k3 =>
    Action k1 config cursor error m a
    -> Action k2 config cursor error m b
    -> Action k3 config cursor error m b
(>>) = (*>)

(<*>) :: forall k1 k2 k3 config cursor error m a b.
    Functor (Action k1 config cursor error m) =>
    Functor (Action k2 config cursor error m) =>
    Join k1 k2 k3 =>
    Action k1 config cursor error m (a -> b)
    -> Action k2 config cursor error m a
    -> Action k3 config cursor error m b
f <*> x = join @k1 @k2 @k3 (fmap (\f' -> fmap f' x) f)

pure :: Monad m => a -> Action 'SureStatic config cursor error m a
pure = BasePrelude.pure

return :: Monad m => a -> Action 'SureStatic config cursor error m a
return = BasePrelude.pure
