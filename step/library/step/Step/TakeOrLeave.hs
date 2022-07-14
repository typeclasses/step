module Step.TakeOrLeave where

data TakeOrLeave b a = Leave b | Take a

collapse :: TakeOrLeave a a -> a
collapse = \case
    Leave x -> x
    Take x -> x
