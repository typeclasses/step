module Step.Interface
  (
    -- * The interface
    Mode (..), Step (..), AdvanceResult (..), stepCast,

    -- * Factories
    commit, reset, peekSomeMaybe, peekCharMaybe, atEnd,

    -- * Vendors
    Buffer (..), bufferedStepper, pureStepper,

  )
  where

import Step.Chunk
import Step.Interface.Core

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
import SupplyChain (Vendor (..), Factory, Supply (..), (>->))
import SupplyChain.Interface.TerminableStream (IsTerminableStream, TerminableStream)
import qualified SupplyChain
import qualified SupplyChain.Interface.TerminableStream as Stream

reset :: forall c m mode. Factory (Step mode c) m ()
reset = SupplyChain.order StepReset

commit :: forall c m. Positive Natural -> Factory (Step 'RW c) m AdvanceResult
commit n = SupplyChain.order (StepCommit n)

peekSomeMaybe :: forall c m mode. Factory (Step mode c) m (Maybe c)
peekSomeMaybe = SupplyChain.order StepNext

peekCharMaybe :: forall c m mode. Chunk c => Factory (Step mode c) m (Maybe (One c))
peekCharMaybe = peekSomeMaybe <&> fmap @Maybe head

atEnd :: forall c m mode. Factory (Step mode c) m Bool
atEnd = peekSomeMaybe <&> isNothing

newtype Buffer c = Buffer{ bufferSeq :: Seq c }

data ViewBuffer c =
    Start
      -- ^ The unseen and unviewed buffers are the same
  | Unviewed (Buffer c)
      -- ^ The unviewed buffer, which may differ from the uncommitted buffer

pureStepper :: forall s up action c. Chunk c => MonadState s action =>
    Lens' s (Buffer c) -> Vendor up (Step 'RW c) action
pureStepper buffer =
    (Stream.nil :: Vendor up (TerminableStream c) action)
    >-> bufferedStepper buffer

bufferedStepper :: forall s up action c. Chunk c => MonadState s action =>
    IsTerminableStream c up =>
    Lens' s (Buffer c) -> Vendor up (Step 'RW c) action
bufferedStepper buffer = go Start
  where
    go :: ViewBuffer c -> Vendor up (Step 'RW c) action
    go unviewed = Vendor \case
        StepReset -> pure $ Supply () (go Start)
        StepNext -> getUnviewedChunks >>= handleNext
        StepCommit n -> getUncommittedChunks >>= handleCommit unviewed n
      where
        getUncommittedChunks :: Factory up action (Seq c)
        getUncommittedChunks = bufferSeq <$> SupplyChain.perform (use buffer)

        getUnviewedChunks :: Factory up action (Seq c)
        getUnviewedChunks = case unviewed of
            Unviewed b -> pure (bufferSeq b)
            Start -> getUncommittedChunks

    handleNext ::
        Seq c -- unviewed chunks
        -> Factory up action
              (Supply up (Step 'RW c) action (Maybe c))
    handleNext = \case
        x :<| xs -> pure $ Supply (Just x) (goUnviewed xs)
        Empty -> SupplyChain.order Stream.nextMaybe >>= \case
            Nothing -> pure $ Supply Nothing (goUnviewed Empty)
            Just x -> feedCommitBuffer x $> Supply (Just x) (goUnviewed Empty)
      where
        goUnviewed :: Seq c -> Vendor up (Step 'RW c) action
        goUnviewed unviewed = go (Unviewed (Buffer unviewed))

        feedCommitBuffer :: c -> Factory up action ()
        feedCommitBuffer x = SupplyChain.perform $
            modifying buffer \(Buffer xs) -> Buffer (xs :|> x)

    handleCommit ::
        ViewBuffer c
        -> Positive Natural -- how much to commit
        -> Seq c -- uncommitted chunks
        -> Factory up action
              (Supply up (Step 'RW c) action AdvanceResult)
    handleCommit unviewed n = \case
        x :<| xs -> case drop n x of
            DropAll ->
                setUncommittedChunks xs $> Supply AdvanceSuccess (go unviewed)
            DropPart{ dropRemainder = x' } ->
                setUncommittedChunks (x' :<| xs) $> Supply AdvanceSuccess (go unviewed)
            DropInsufficient{ dropShortfall = n' } -> handleCommit unviewed n' xs
        Empty -> SupplyChain.order Stream.nextMaybe >>= \case
            Nothing -> pure $ Supply YouCanNotAdvance{ shortfall = n } (go unviewed)
            Just x -> handleCommit unviewed' n (x :<| Empty)
              where
                unviewed' = case unviewed of
                    Start -> Unviewed (Buffer (x :<| Empty))
                    Unviewed (Buffer xs) -> Unviewed (Buffer (x :<| xs))
      where
        setUncommittedChunks :: Seq c -> Factory up action ()
        setUncommittedChunks xs = SupplyChain.perform $ assign buffer (Buffer xs)
