module Step.Cursor.ChunkStream
  (
    {- * Core -} Stream, stream, next,
    {- * Extras -} while, untrivialize, rebase, record, list,
  )
  where

import Step.Internal.Prelude hiding (while)

import qualified Step.Cursor.Stream as S

import Step.Cursor.StreamCompletion (StreamCompletion (..))

import Step.Nontrivial (Nontrivial)
import qualified Step.Nontrivial as Nontrivial
import qualified Step.Nontrivial.TakeWhile as TakeWhile

newtype Stream m xs x = Stream{ chunkStream :: S.Stream m (Nontrivial xs x) }

stream :: m (Maybe (Nontrivial xs x)) -> Stream m xs x
stream = Stream . S.stream

next :: Stream m xs x -> m (Maybe (Nontrivial xs x))
next = S.next . chunkStream

untrivialize :: Monad m => ListLike xs x => S.Stream m xs -> Stream m xs x
untrivialize = Stream . S.mapMaybe Nontrivial.refine

rebase :: (forall a. m1 a -> m2 a) -> Stream m1 xs x -> Stream m2 xs x
rebase f = Stream . S.rebase f . chunkStream

record :: Monad m => (Nontrivial xs x -> StateT s m ()) -> Stream m xs x -> Stream (StateT s m) xs x
record f = Stream . S.record f . chunkStream

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
