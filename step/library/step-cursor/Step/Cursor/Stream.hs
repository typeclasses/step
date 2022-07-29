module Step.Cursor.Stream
  (
    {- * Core -} Stream, stream, next,
    {- * Extras -} while, untrivialize, rebaseStream, record, list, streamChoice, mapMaybe,
  )
  where

import Step.Internal.Prelude hiding (while)

import Step.Cursor.StreamCompletion (StreamCompletion (..))

import Step.Nontrivial (Nontrivial)
import qualified Step.Nontrivial as Nontrivial
import qualified Step.Nontrivial.TakeWhile as TakeWhile

newtype Stream m xs x = Stream{ next :: m (Maybe (Nontrivial xs x)) }

stream :: m (Maybe (Nontrivial xs x)) -> Stream m xs x
stream = Stream

untrivialize :: Monad m => ListLike xs x => m (Maybe xs) -> Stream m xs x
untrivialize xs = Stream $ fix \r ->
    xs >>= \case
        Nothing -> return Nothing
        Just x -> case Nontrivial.refine x of { Just y -> return (Just y); Nothing -> r }

mapMaybe :: Monad m => (Nontrivial xs x -> Maybe (Nontrivial ys y)) -> Stream m xs x -> Stream m ys y
mapMaybe ok xs = Stream $ fix \r ->
    next xs >>= \case
        Nothing -> return Nothing
        Just x -> case ok x of { Just y -> return (Just y); Nothing -> r }

rebaseStream :: (forall a. m1 a -> m2 a) -> Stream m1 xs x -> Stream m2 xs x
rebaseStream f Stream{ next } = Stream{ next = f next }

record :: Monad m => (Nontrivial xs x -> StateT s m ()) -> Stream m xs x -> Stream (StateT s m) xs x
record add xs = Stream do
    step <- lift (next xs)
    traverse_ add step
    return step

while :: Monad m => ListLike xs x =>
    Predicate x
    -> Stream m xs x
    -> Stream (StateT StreamCompletion m) xs x
while ok xs = stream $ get >>= \case
    Done -> return Nothing
    MightBeMore -> lift (next xs) >>= \case
        Nothing -> put Done $> Nothing
        Just x -> case Nontrivial.takeWhile ok x of
            TakeWhile.All -> return (Just x)
            TakeWhile.None -> put Done $> Nothing
            TakeWhile.Prefix y -> put Done $> Just y

list :: Monad m => Stream (StateT [Nontrivial xs x] m) xs x
list = stream $ StateT \case [] -> return (Nothing, []); (x : xs) -> return (Just x, xs)

streamChoice :: Monad m => Stream m xs x -> Stream m xs x -> Stream m xs x
streamChoice xs ys = stream $ next xs >>= \case
    Just x -> return (Just x)
    Nothing -> next ys
