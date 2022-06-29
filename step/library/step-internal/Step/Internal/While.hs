module Step.Internal.While where

import BasePrelude
import Function

while :: Monad m =>
    (a -> Bool)    -- ^ While this predicate holds, keep going.
    -> (a -> m a)  -- ^ State modification that will happen repeatedly.
    -> a           -- ^ Initial value
    -> m a
while continue step =
    fix \r x ->
        if continue x then step x >>= r else return x

until :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m a
until continue step = while (not . continue) step
