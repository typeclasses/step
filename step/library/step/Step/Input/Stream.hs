{-# language RankNTypes #-}

module Step.Input.Stream where

import Step.Internal.Prelude

newtype Stream m a = Stream{ next :: m (Maybe a) }

changeBase :: (forall x. m1 x -> m2 x) -> Stream m1 a -> Stream m2 a
changeBase f Stream{ next } = Stream{ next = f next }

record :: Monad m => (a -> s -> s) -> Stream m a -> Stream (StateT s m) a
record add = r
  where
    r xs = Stream do
        step <- lift (next xs)
        traverse_ (modify' . add) step
        return step

filter :: Monad m => (a -> Bool) -> Stream m a -> Stream m a
filter ok xs = Stream $ fix \r ->
    next xs >>= \case
        Nothing -> return Nothing
        Just x -> if ok x then return (Just x) else r

mapMaybe :: Monad m => (a -> Maybe b) -> Stream m a -> Stream m b
mapMaybe ok xs = Stream $ fix \r ->
    next xs >>= \case
        Nothing -> return Nothing
        Just x -> case ok x of { Just y -> return (Just y); Nothing -> r }

list :: Monad m => Stream (StateT [a] m) a
list = Stream{ next = StateT \case [] -> return (Nothing, []); (x : xs) -> return (Just x, xs) }
