module Step.TakeOrLeave where

import Maybe (Maybe (..))

data TakeOrLeave b a = Leave b | Take a

collapse :: TakeOrLeave a a -> a
collapse = \case
    Leave x -> x
    Take x -> x

fromTake :: TakeOrLeave b a -> Maybe a
fromTake = \case
    Leave _ -> Nothing
    Take x -> Just x
