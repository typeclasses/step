module Step.Document.Do where

import Step.Kind.Base (StepKind (..))

import Step.Document.Parser (Parser, generalizeTo, Bind, bind)

import qualified BasePrelude
import BasePrelude (Monad)

(>>=) :: Bind pt1 pt2 pt3 =>
    Parser text pt1 m a
    -> (a -> Parser text pt2 m b)
    -> Parser text pt3 m b
(>>=) = bind

(<*) :: (Bind pt2 pt3 pt4, Bind pt5 'Certainty0 pt3, Monad m) =>
    Parser text pt2 m b -> Parser text pt5 m a -> Parser text pt4 m b
a <* b = a >>= \x -> b >>= \_ -> return x

(*>) :: Bind pt1 pt2 pt3 =>
    Parser text pt1 m a -> Parser text pt2 m b -> Parser text pt3 m b
a *> b = a >>= \_ -> b

(<*>) :: (Bind pt2 pt3 pt4, Bind pt5 'Certainty0 pt3, Monad m) =>
    Parser text pt2 m (t -> b) -> Parser text pt5 m t -> Parser text pt4 m b
f <*> x = f >>= \f' -> x >>= \x' -> return (f' x')

(<$>) :: (Bind pt1 'Certainty0 pt3, Monad m) =>
    (t -> b) -> Parser text pt1 m t -> Parser text pt3 m b
f <$> x = x >>= \x' -> return (f x')

return :: Monad m => a -> Parser text 'Certainty0 m a
return = BasePrelude.return
