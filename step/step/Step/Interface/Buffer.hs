module Step.Interface.Buffer
  (
    Buffer (..), bufferedStepper, pureStepper,
    doubleBuffer,
  )
  where

import Step.Chunk
import Step.Interface
import qualified Step.Interface.Core as I

-- The basics
import Data.Bool (Bool (..))
import Data.Maybe (Maybe (..), isNothing)
import Data.Functor (Functor (..), (<&>), ($>), (<$>))
import Data.Function (($))
import Control.Monad (Monad (..))
import Control.Applicative (Applicative (..))

-- Containers
import Data.Sequence (Seq (..))

-- Optics
import Optics (Lens', use, assign, modifying)

-- Math
import Numeric.Natural (Natural)
import NatOptics.Positive.Unsafe (Positive)

-- Transformers
import Control.Monad.State.Strict (MonadState)

-- Streaming
import SupplyChain (Vendor (..), Job, Referral (..), (>->), order)
import SupplyChain.Interface.TerminableStream (IsTerminableStream, TerminableStream)
import qualified SupplyChain
import qualified SupplyChain.Interface.TerminableStream as Stream

newtype Buffer c = Buffer{ bufferSeq :: Seq c }

data ViewBuffer c =
    Start
      -- ^ The unseen and unviewed buffers are the same
  | Unviewed (Buffer c)
      -- ^ The unviewed buffer, which may differ from the uncommitted buffer

pureStepper :: forall s up action c. Chunk c => MonadState s action =>
    Lens' s (Buffer c) -> Vendor up (CommittableChunkStream c) action
pureStepper buffer =
    (Stream.nil :: Vendor up (TerminableStream c) action)
    >-> bufferedStepper buffer

{-| Turns an unbuffered stream (the 'IsTerminableStream' interface)
    into a buffered stream (the 'Step' interface).

    A buffer stored in the 'MonadState' context, at a position identified
    by the given 'Lens'' parameter, holds any input that has been read from
    the unbuffered stream but has not yet been committed. The remaining input,
    then, consists of anything that is in the buffer, followed by anything
    that is yet to be obtained from the unbuffered stream.
-}
bufferedStepper :: forall s up action c. Chunk c => MonadState s action =>
    IsTerminableStream c up =>
    Lens' s (Buffer c) -> Vendor up (CommittableChunkStream c) action
bufferedStepper buffer = go Start
  where
    -- The "unviewed" buffer is internal state only, not externally accessible.
    go :: ViewBuffer c -> Vendor up (CommittableChunkStream c) action
    go unviewed = Vendor \case
        I.Reset -> pure $ Referral () (go Start)
        I.NextMaybe -> getUnviewedChunks >>= handleNext
        I.Commit n -> getUncommittedChunks >>= handleCommit unviewed n
      where
        getUncommittedChunks :: Job up action (Seq c)
        getUncommittedChunks = bufferSeq <$> SupplyChain.perform (use buffer)

        getUnviewedChunks :: Job up action (Seq c)
        getUnviewedChunks = case unviewed of
            Unviewed b -> pure (bufferSeq b)
            Start -> getUncommittedChunks

    handleNext ::
        Seq c -- unviewed chunks
        -> Job up action
              (Referral up (CommittableChunkStream c) action (Maybe c))
    handleNext = \case
        x :<| xs -> pure $ Referral (Just x) (goUnviewed xs)
        Empty -> order Stream.nextMaybe >>= \case
            Nothing -> pure $ Referral Nothing (goUnviewed Empty)
            Just x -> feedCommitBuffer x $> Referral (Just x) (goUnviewed Empty)
      where
        goUnviewed :: Seq c -> Vendor up (CommittableChunkStream c) action
        goUnviewed unviewed = go (Unviewed (Buffer unviewed))

        feedCommitBuffer :: c -> Job up action ()
        feedCommitBuffer x = SupplyChain.perform $
            modifying buffer \(Buffer xs) -> Buffer (xs :|> x)

    handleCommit ::
        ViewBuffer c
        -> Positive Natural -- how much to commit
        -> Seq c -- uncommitted chunks
        -> Job up action
              (Referral up (CommittableChunkStream c) action AdvanceResult)
    handleCommit unviewed n = \case
        x :<| xs -> case drop n x of
            DropAll ->
                setUncommittedChunks xs $> Referral AdvanceSuccess (go unviewed)
            DropPart{ dropRemainder = x' } ->
                setUncommittedChunks (x' :<| xs) $> Referral AdvanceSuccess (go unviewed)
            DropInsufficient{ dropShortfall = n' } -> handleCommit unviewed n' xs
        Empty -> order Stream.nextMaybe >>= \case
            Nothing -> pure $ Referral YouCanNotAdvance{ shortfall = n } (go unviewed)
            Just x -> handleCommit unviewed' n (x :<| Empty)
              where
                unviewed' = case unviewed of
                    Start -> Unviewed (Buffer (x :<| Empty))
                    Unviewed (Buffer xs) -> Unviewed (Buffer (x :<| xs))
      where
        setUncommittedChunks :: Seq c -> Job up action ()
        setUncommittedChunks xs = SupplyChain.perform $ assign buffer (Buffer xs)

data DoubleBuffer c = DoubleBuffer{ dbCommit :: Seq c, dbView :: Seq c }

doubleBuffer :: forall c up action. Chunk c => IsTerminableStream c up =>
    Vendor up (CommittableChunkStream c) action
doubleBuffer = go (DoubleBuffer Empty Empty)
  where
    go :: DoubleBuffer c -> Vendor up (CommittableChunkStream c) action
    go s = Vendor \case
        I.Reset -> pure $ Referral () $ go s{ dbView = dbCommit s }

        I.NextMaybe ->
            case dbView s of
                x :<| xs -> pure $ Referral (Just x) $ go s{ dbView = xs }
                Empty -> SupplyChain.order Stream.nextMaybe >>= \case
                    Nothing -> pure $ Referral Nothing $ go s
                    Just x -> pure $ Referral (Just x) $ go s{ dbCommit = dbCommit s :|> x }

        c@(I.Commit n) ->
            case dbCommit s of
                x :<| xs -> case drop n x of
                    DropAll -> pure $ Referral AdvanceSuccess $ go s{ dbCommit = xs }
                    DropPart{ dropRemainder = x' } -> pure $ Referral AdvanceSuccess $ go s{ dbCommit = x' :<| xs }
                    DropInsufficient{ dropShortfall = n' } -> go s{ dbCommit = xs } `handle` I.Commit n'
                Empty -> SupplyChain.order Stream.nextMaybe >>= \case
                    Nothing -> pure $ Referral YouCanNotAdvance{ shortfall = n } $ go s
                    Just x -> go s{ dbCommit = Empty :|> x, dbView = dbView s :|> x } `handle` c
