{-# language ViewPatterns #-}

module Step.While where

import Step.Internal.Prelude hiding (while)

import Step.Nontrivial (Nontrivial)
import qualified Step.Nontrivial as Nontrivial
import qualified Step.Nontrivial.TakeWhile as Nontrivial.TakeWhile

import Step.Input.AdvanceResult
import qualified Step.Input.AdvanceResult as Advance

import Step.Input.Stream (Stream (..))
import qualified Step.Input.Stream as Stream

import Step.Buffer.Base (Buffer)
import qualified Step.Buffer.Base as Buffer

import Step.Input.Cursor (Session (Session))
import qualified Step.Input.Cursor as Session

import Signed (Signed (..))
import qualified Signed

import qualified Maybe

import qualified Positive
import qualified Positive.Math as Positive

data While text char =
  While
    { completion :: Stream.Completion  -- ^ Should we read any more from upstream
    , inputBuffer :: Buffer text char  -- ^ Input that has been read from upstream but not seen downstream.
                                       --   Anything in the buffer has been verified all okay.
    , commitBuffer :: Buffer text char -- ^ Input that has been read from upstream but not committed.
                                       --   Anything in the buffer has been verified all okay.
    }

completionLens :: Lens (While text char) (While text char) Stream.Completion Stream.Completion
completionLens = lens completion \x y -> x{ completion = y }

inputBufferLens :: Lens (While text char) (While text char) (Buffer text char) (Buffer text char)
inputBufferLens = lens inputBuffer \x y -> x{ inputBuffer = y }

commitBufferLens :: Lens (While text char) (While text char) (Buffer text char) (Buffer text char)
commitBufferLens = lens commitBuffer \x y -> x{ commitBuffer = y }

init :: While text char
init = While{ completion = Stream.MightBeMore, inputBuffer = Buffer.empty, commitBuffer = Buffer.empty }

while :: forall m text char. Monad m => ListLike text char =>
    Predicate char -> Session text char m -> Session text char m
while ok
  Session
    { Session.run = runUpstream :: forall a. m' a -> m a
    , Session.commit = commitUpstream :: Positive Natural -> m' AdvanceResult
    , Session.input = (Stream.chunkedWhile ok -> inputUpstream :: Stream (StateT Stream.Completion m') (Nontrivial text char))
    } =
    Session{ Session.run, Session.commit, Session.input }
  where
    run :: StateT (While text char) m' a -> m a
    run a = runUpstream (evalStateT a init)

    input :: Stream (StateT (While text char) m') (Nontrivial text char)
    input = _

    commit :: Positive Natural -> StateT (While text char) m' AdvanceResult
    commit n = _

  --   Stream{ next }
  -- where
  --   next :: StateT (While text char) m (Maybe (Nontrivial text char))
  --   next = zoom bufferLens SizedBuffer.takeChunk >>= \case
  --       Just x -> yield x
  --       Nothing -> use completionLens >>= \case
  --           Done -> return Nothing
  --           MightBeMore -> fetch

  --   fetch :: StateT (While text char) m (Maybe (Nontrivial text char))
  --   fetch = lift (Stream.next upstream) >>= \case
  --       Nothing -> assign completionLens Done $> Nothing
  --       Just x -> case Nontrivial.takeWhile ok x of
  --           Nontrivial.TakeWhile.None -> assign completionLens Done $> Nothing
  --           Nontrivial.TakeWhile.All -> yield x
  --           Nontrivial.TakeWhile.Prefix y -> assign completionLens Done *> yield y

  --   yield :: Nontrivial text char -> StateT (While text char) m (Maybe (Nontrivial text char))
  --   yield x = modifying uncommittedLens (+ Nontrivial.lengthInt x) $> Just x

-- commit :: ListLike text char => Monad m =>
--     Predicate char
--     -> Stream m (Nontrivial text char)
--     -> (Positive Natural -> m AdvanceResult)
--     -> Positive Natural
--     -> StateT (While text char) m AdvanceResult
-- commit ok inputUpstream commitUpstream =
--     _

-- fix \r n ->
--     use (uncommittedLens % Signed.intNatIso) >>= \case
--         Signed.Plus amountUncommitted -> case Positive.minus amountUncommitted n of
--             Signed.Plus remainderUncommitted -> do
--                 assign (uncommittedLens % Signed.intNatIso) (Signed.Plus remainderUncommitted)
--                 lift (commitUpstream n)
--             Signed.Zero -> do
--                 assign uncommittedLens 0
--                 lift (commitUpstream n)
--             Signed.Minus n' -> do
--                 assign uncommittedLens 0
--                 lift (commitUpstream amountUncommitted)
--                 r n'
--         Signed.Zero -> lift (Stream.next inputUpstream) >>= \case
--             Nothing -> do
--                 assign completionLens Done
--                 return Advance.InsufficientInput{ shortfall = n }
--             Just x -> case Nontrivial.takeWhile ok x of
--                 Nontrivial.TakeWhile.None -> do
--                     assign completionLens Done
--                     return Advance.InsufficientInput{ shortfall = n }
--                 Nontrivial.TakeWhile.All -> do
--                     modifying bufferLens (SizedBuffer.|> x)
--                     modifying uncommittedLens (\u -> u - review Positive.intPrism n)
--                     return Advance.Success
--                 Nontrivial.TakeWhile.Prefix a -> do
--                     modifying bufferLens (SizedBuffer.|> x)
--                     -- modifying uncommittedLens (\u ->
