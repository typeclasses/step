module Step.Cursor.Stream
  (
    {- * Core -} Stream (..), stream, streamRST,
    {- * Extras -}
    while, untrivialize,
    -- record,
    list, streamChoice, mapMaybe,
  )
  where

import Step.Internal.Prelude hiding (while)

import Step.Cursor.StreamCompletion (StreamCompletion (..))

import Step.Nontrivial (Nontrivial)
import qualified Step.Nontrivial as Nontrivial
import qualified Step.Nontrivial.TakeWhile as TakeWhile

import Step.RST

import Optics

newtype Stream r s m xs x =
    Stream{ next :: RST r s m (Maybe (Nontrivial xs x)) }

streamRST :: Iso
  (Stream r1 s1 m1 xs1 x1)
  (Stream r2 s2 m2 xs2 x2)
  (RST r1 s1 m1 (Maybe (Nontrivial xs1 x1)))
  (RST r2 s2 m2 (Maybe (Nontrivial xs2 x2)))
streamRST = iso next Stream

stream :: Monad m => m (Maybe (Nontrivial xs x)) -> Stream r s m xs x
stream = Stream .  lift

untrivialize :: Monad m => ListLike xs x => m (Maybe xs) -> Stream r s m xs x
untrivialize xs = Stream $ lift $ fix \r ->
    xs >>= \case
        Nothing -> return Nothing
        Just x -> case Nontrivial.refine x of { Just y -> return (Just y); Nothing -> r }

mapMaybe :: Monad m => (Nontrivial xs x -> Maybe (Nontrivial ys y)) -> Stream r s m xs x -> Stream r s m ys y
mapMaybe ok xs = Stream $ fix \r ->
    next xs >>= \case
        Nothing -> return Nothing
        Just x -> case ok x of { Just y -> return (Just y); Nothing -> r }

-- record :: Monad m => (Nontrivial xs x -> StateT s m ()) -> Stream r s m xs x -> Stream r ateT s m) xs x
-- record add xs = Stream do
--     step <- lift (next xs)
--     traverse_ add step
--     return step

while :: forall m xs x r s. Monad m => ListLike xs x =>
    Predicate x
    -> Stream r s m xs x
    -> Stream r (StreamCompletion, s) m xs x
while ok xs = Stream $ use _1 >>= \case
    Done -> return Nothing
    MightBeMore -> zoom _2 (next xs) >>= \case
        Nothing -> assign _1 Done $> Nothing
        Just x -> case Nontrivial.takeWhile ok x of
            TakeWhile.All -> return (Just x)
            TakeWhile.None -> assign _1 Done $> Nothing
            TakeWhile.Prefix y -> assign _1 Done $> Just y

list :: Monad m => Stream r [Nontrivial xs x] m xs x
list = Stream $ get >>= \case
    [] -> return Nothing
    x : xs -> put xs $> Just x

streamChoice :: Monad m => Stream r s m xs x -> Stream r s m xs x -> Stream r s m xs x
streamChoice xs ys = Stream $ next xs >>= \case
    Just x -> return (Just x)
    Nothing -> next ys
