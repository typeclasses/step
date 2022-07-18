module Step.TakeOrLeave where

import Either (Either (..))
import Maybe (Maybe (..))
import Optics (Prism, prism)

data TakeOrLeave b a = Leave b | Take a

collapse :: TakeOrLeave a a -> a
collapse = \case
    Leave x -> x
    Take x -> x

fromTake :: TakeOrLeave b a -> Maybe a
fromTake = \case
    Leave _ -> Nothing
    Take x -> Just x

leavePrism :: Prism (TakeOrLeave b1 a2) (TakeOrLeave b2 a2) b1 b2
leavePrism = prism Leave \case{ Leave x -> Right x; Take x -> Left (Take x) }
