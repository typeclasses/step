module Step.While where

import Step.Internal.Prelude hiding (while)

import Step.Nontrivial.Base
import qualified Step.Nontrivial.List as Nontrivial
import qualified Step.Nontrivial.TakeWhile as Nontrivial (takeWhile, TakeWhile)
import qualified Step.Nontrivial.TakeWhile as Nontrivial.TakeWhile

import Step.Input.AdvanceResult

import Step.Input.Stream
import qualified Step.Input.Stream as Stream

import Step.Buffer.Base (Buffer)
import qualified Step.Buffer.Base as Buffer

import Step.Buffer.Session (BufferSession)
import qualified Step.Buffer.Session as BufferSession

import Step.Input.Cursor (Session (..))

import Signed (Signed (..))
import qualified Signed

data Completion = Done | MightBeMore

data While text char = While{ completion :: Completion, mismatch :: Mismatch text char }

data Mismatch text char = Black Natural | Red (Positive Natural) (Buffer text char)

completionLens :: Lens (While text char) (While text char) Completion Completion
completionLens = lens completion \x y -> x{ completion = y }

mismatchLens :: Lens (While text1 char1) (While text2 char2) (Mismatch text1 char1) (Mismatch text2 char2)
mismatchLens = lens mismatch \x y -> x{ mismatch = y }

-- data BufferResult = BufferedMore | NoMoreToBuffer

while :: forall m text char. Monad m => ListLike text char =>
    (char -> Bool) -> Session text char m -> Session text char m
while ok Session{ run = runUpstream :: forall a. m' a -> m a, commit = commitUpstream, input = inputUpstream } =
    Session{ run = run', commit = commit', input = input' }
  where
    run' :: StateT (While text char) m' a -> m a
    run' = runUpstream . _

    commit' :: Positive Natural -> StateT (While text char) m' AdvanceResult
    commit' n = _

    input' :: Stream (StateT (While text char) m') (Nontrivial text char)
    input' =
      Stream
        { next = use mismatchLens >>= \case
              Black n -> use completionLens >>= \case
                  Done -> return Nothing
                  MightBeMore -> lift (Stream.next inputUpstream) >>= \case
                      Nothing -> put (While Done (Black n)) $> Nothing
                      Just x -> case Nontrivial.takeWhile ok x of
                          Nontrivial.TakeWhile.None -> put (While Done (Black n)) $> Nothing
                          Nontrivial.TakeWhile.All -> put (While MightBeMore (Black (n + Nontrivial.lengthNat x))) $> Just x
                          Nontrivial.TakeWhile.Prefix y -> put (While Done (Black (n + Nontrivial.lengthNat y))) $> Just y
              Red n b -> _
        }

            -- xm <-
            --     zoom (bufferSessionLens % BufferSession.unseenLens) Buffer.takeChunk >>= \case
            --         Just x -> return (Just x)
            --         Nothing -> bufferMore >>= \case
            --             NoMoreToBuffer -> return Nothing
            --             BufferedMore -> zoom (bufferSessionLens % BufferSession.unseenLens) Buffer.takeChunk
            -- case xm of
            --     Nothing -> return Nothing
            --     Just x -> case Nontrivial.takeWhile ok x of
            --         Nothing -> assign doneLens True $> Nothing
            --         Just y -> do
            --             modifying amountReadLens (+ Nontrivial.lengthNat y)
            --             use amountReadLens >>= assign amountCheckedLens
            --             unless (Nontrivial.length y == Nontrivial.length x) $ assign doneLens True
            --             return (Just y)

    -- bufferMore :: StateT (While text char) m' BufferResult
    -- bufferMore = lift (Stream.next inputUpstream) >>= \case
    --     Nothing -> return NoMoreToBuffer
    --     Just x -> modifying inputBufferLens (Buffer.|> x) $> BufferedMore
