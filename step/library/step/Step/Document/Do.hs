module Step.Document.Do where

import Step.Kind.Base (Join, StepKind (..))

import Step.Document.Parser (Parser, generalizeTo, Is)

import qualified BasePrelude
import BasePrelude (Monad)

(>>=) ::
    forall pt1 pt2 pt3 text a b m.
    (Join pt1 pt2 ~ pt3, Is pt1 pt3, Is pt2 pt3) =>
    Monad (Parser text pt3 m) =>
    Monad m =>
    Parser text pt1 m a
    -> (a -> Parser text pt2 m b)
    -> Parser text pt3 m b
a >>= f =
    generalizeTo @pt3 a
    BasePrelude.>>= \x ->
    generalizeTo @pt3 (f x)

return :: Monad m => a -> Parser text 'Certainty0 m a
return = BasePrelude.return
