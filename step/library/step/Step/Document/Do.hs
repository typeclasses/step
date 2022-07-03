module Step.Document.Do where

import Step.Kind.Base (StepKind (..))

import Step.Document.Parser (Parser (CertainParser), generalizeTo, PolyJoin, polyJoin)

import qualified BasePrelude
import BasePrelude (Monad, Functor, Applicative)

join :: forall pt1 pt2 pt3 text m a. PolyJoin pt1 pt2 pt3 =>
    Parser text pt1 m (Parser text pt2 m a) -> Parser text pt3 m a
join = polyJoin @pt1 @pt2 @pt3

(>>=) :: forall pt1 pt2 pt3 text m a b. Functor m => PolyJoin pt1 pt2 pt3 =>
    Parser text pt1 m a -> (a -> Parser text pt2 m b) -> Parser text pt3 m b
x >>= f = join (fmap f x)

(<*) :: forall pt1 pt2 pt3 text m a b. Functor m => PolyJoin pt1 pt2 pt3 =>
    Parser text pt1 m a -> Parser text pt2 m b -> Parser text pt3 m a
a <* b = join @pt1 @pt2 @pt3 (fmap (\x -> fmap (\_ -> x) b) a)

(*>) :: forall pt1 pt2 pt3 text m a b. Functor m => PolyJoin pt1 pt2 pt3 =>
    Parser text pt1 m a -> Parser text pt2 m b -> Parser text pt3 m b
a *> b = join @pt1 @pt2 @pt3 (fmap (\_ -> b) a)

(>>) :: forall pt1 pt2 pt3 text m a b. Functor m => PolyJoin pt1 pt2 pt3 =>
    Parser text pt1 m a -> Parser text pt2 m b -> Parser text pt3 m b
(>>) = (*>)

(<*>) :: forall pt1 pt2 pt3 text m a b. (PolyJoin pt1 pt2 pt3, Functor m) =>
    Parser text pt1 m (a -> b)
    -> Parser text pt2 m a
    -> Parser text pt3 m b
f <*> x = join @pt1 @pt2 @pt3 (fmap (\f' -> fmap f' x) f)

fmap :: Functor m => (a -> b) -> Parser text pt m a -> Parser text pt m b
fmap = BasePrelude.fmap

pure :: Monad m => a -> Parser text 'SureStatic m a
pure x = CertainParser \_config -> BasePrelude.pure x

return :: Monad m => a -> Parser text 'SureStatic m a
return = pure
