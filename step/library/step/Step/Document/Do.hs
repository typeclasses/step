module Step.Document.Do where

import Step.Kind.Base (StepKind (..))

import Step.Document.Parser (Parser, generalizeTo, PolyJoin, polyJoin)

import qualified BasePrelude
import BasePrelude (Monad, Functor)

join :: PolyJoin pt1 pt2 pt3 =>
    Parser text pt1 m (Parser text p2 m a) -> Parser text p3 m a
join = polyJoin

(<*) :: PolyJoin pt1 pt2 pt3 =>
    Parser text pt1 m a -> Parser text pt2 m b -> Parser text pt3 m a
a <* b = join (fmap (\x -> fmap (\_ -> x) b) a)

(*>) :: PolyJoin pt1 pt2 pt3 =>
    Parser text pt1 m a -> Parser text pt2 m b -> Parser text pt3 m b
a *> b = join (fmap (\_ -> b) a)

(<*>) :: (PolyJoin pt1 pt2 pt3, Functor m) =>
    Parser text pt1 m (a -> b)
    -> Parser text p2 m a
    -> Parser text p3 m b
f <*> x = join (fmap (\f' -> fmap f' x) f)

fmap :: Functor m => (a -> b) -> Parser text pt m a -> Parser text pt m b
fmap = BasePrelude.fmap

return :: Monad m => a -> Parser text 'SureStatic m a
return = BasePrelude.return
