module Step.Util.While where

import Step.Util.Prelude

while :: Monad m =>
    (a -> Bool)    -- ^ While this predicate holds, keep going.
    -> (a -> m a)  -- ^ State modification that will happen repeatedly.
    -> a           -- ^ Initial value
    -> m a
while continue step =
    fix \r x ->
        if continue x then step x >>= r else return x
