module Step.Input.Stream
  (
    Stream (..),
    changeBase,
    record,
    filter,
    mapMaybe,
    list,
    chunkedWhile,
    Completion (..),
  )
  where

import Step.Internal.Prelude

import Step.Nontrivial (Nontrivial)
import qualified Step.Nontrivial as Nontrivial
import qualified Step.Nontrivial.TakeWhile as Nontrivial.TakeWhile

newtype Stream m a = Stream{ next :: m (Maybe a) }

changeBase :: (forall x. m1 x -> m2 x) -> Stream m1 a -> Stream m2 a
changeBase f Stream{ next } = Stream{ next = f next }

record :: MonadTrans t => Monad m => MonadState s (t m) =>
    (a -> t m ()) -> Stream m a -> Stream (t m) a
record add = r
  where
    r xs = Stream do
        step <- lift (next xs)
        traverse_ add step
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

data Completion = Done | MightBeMore

chunkedWhile :: Monad m => ListLike xs x =>
    Predicate x
    -> Stream m (Nontrivial xs x)
    -> Stream (StateT Completion m) (Nontrivial xs x)
chunkedWhile ok xs = Stream $ get >>= \case
    Done -> return Nothing
    MightBeMore -> lift (next xs) >>= \case
        Nothing -> put Done $> Nothing
        Just x -> case Nontrivial.takeWhile ok x of
            Nontrivial.TakeWhile.All -> return (Just x)
            Nontrivial.TakeWhile.None -> put Done $> Nothing
            Nontrivial.TakeWhile.Prefix y -> put Done $> Just y
