module Step.Action.SeparateTypes where

import Step.Internal.Prelude

import Step.Action.Kinds

---

tryAnySure :: Functor m => Any config cursor error m a -> Sure config cursor error m (Maybe a)
tryAnySure (Any p) = Sure \c -> p c <&> \case
    Left _ -> Nothing
    Right x -> Just x

---
