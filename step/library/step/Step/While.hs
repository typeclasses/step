module Step.While where

import Step.Internal.Prelude hiding (while)

import qualified Step.Nontrivial as Nontrivial
import qualified Step.Nontrivial.TakeWhile as Nontrivial.TakeWhile

import Step.Cursor (Cursor (Cursor), StreamCompletion, Stream, AdvanceResult (..))
import qualified Step.Cursor as Cursor

import Step.Buffer.Buffer (Buffer, chunks)
import Step.Buffer.BufferState (BufferState, runBufferState)

import qualified Step.Buffer.Buffer as Buffer
import qualified Step.Buffer.BufferState as BufferState

data While text char =
  While
    { completion :: StreamCompletion -- ^ Should we read any more from upstream
    , uncommitted :: Integer -- ^ How many characters have been sent downstream but not committed
    , buffer :: Buffer text char -- ^ Input that has been read from upstream but not seen downstream
    }

completionLens :: Lens (While text char) (While text char) StreamCompletion StreamCompletion
completionLens = lens completion \x y -> x{ completion = y }

uncommittedLens :: Lens (While text char) (While text char) Integer Integer
uncommittedLens = lens uncommitted \x y -> x{ uncommitted = y }

bufferLens :: Lens (While text1 char1) (While text2 char2) (Buffer text1 char1) (Buffer text2 char2)
bufferLens = lens buffer \x y -> x{ buffer = y }

init :: While text char
init = While{ completion = Cursor.MightBeMore, uncommitted = 0, buffer = [] }

while :: forall m m' xs x. Monad m => Monad m' => ListLike xs x =>
    Predicate x -> Cursor xs x m m' -> Cursor xs x m (StateT (While xs x) m')
while ok
  Cursor
    { Cursor.run = runUpstream :: forall a. m' a -> m a
    , Cursor.commit = commitUpstream
    , Cursor.input = inputUpstream
    } =
      Cursor
        { Cursor.run = \a -> runUpstream (evalStateT a init)
        , Cursor.commit = commit ok commitUpstream
        , Cursor.input = input ok inputUpstream
        }

run :: Monad m => StateT (While text char) m a -> m a
run = _

input :: ListLike text char => Monad m =>
    Predicate char
    -> Stream m text char
    -> Stream (StateT (While text char) m) text char
input ok upstream = Cursor.stream do
    xm <-
        zoom bufferLens (runBufferState BufferState.takeChunk) >>= \case
            Just x -> return (Just x)
            Nothing -> use completionLens >>= \case
                Cursor.Done -> return Nothing
                Cursor.MightBeMore -> lift (Cursor.next upstream) >>= \case
                    Nothing -> assign completionLens Cursor.Done $> Nothing
                    Just x -> return (Just x)
    case xm of
        Nothing -> return Nothing
        Just x -> case Nontrivial.takeWhile ok x of
            Nontrivial.TakeWhile.None -> do
                assign completionLens Cursor.Done
                return Nothing
            Nontrivial.TakeWhile.All -> do
                modifying uncommittedLens (+ Nontrivial.lengthInt x)
                return (Just x)
            Nontrivial.TakeWhile.Prefix y -> do
                modifying uncommittedLens (+ Nontrivial.lengthInt y)
                assign completionLens Cursor.Done
                return (Just y)

commit :: ListLike text char => Monad m =>
    Predicate char
    -> (Positive Natural -> m AdvanceResult)
    -> Positive Natural
    -> StateT (While text char) m AdvanceResult
commit ok upstream n = _
