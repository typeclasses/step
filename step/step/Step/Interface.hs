module Step.Interface
  (
    -- * The interface
    Mode (..), Step (..), AdvanceResult (..), stepCast,

    -- * Factories
    commit, reset, peekSomeMaybe, peekCharMaybe, atEnd,

    -- * Vendors
    Buffer (..), bufferedStepper, pureStepper,

    -- * Walk
    Walk (..),

  )
  where

import Step.Chunk
import Step.Interface.Core

-- The basics
import Data.Bool (Bool (..))
import Data.Maybe (Maybe (..), maybe, isNothing)
import Data.Functor (Functor (..), (<&>), ($>), (<$>))
import Data.Function (($), (.))
import Data.Either (Either (..))
import Control.Monad (Monad (..))
import Control.Applicative (Applicative (..))

-- Containers
import Data.Sequence (Seq (..))

-- Optics
import Optics (preview, Lens', use, assign, modifying)

-- Math
import Numeric.Natural (Natural)
import NatOptics.Positive.Unsafe (Positive (PositiveUnsafe))
import qualified NatOptics.Positive as Positive
import qualified NatOptics.Positive.Math as Positive
import qualified NatOptics.Signed as Signed

-- Transformers
import Control.Monad.Reader (MonadReader)
import qualified Control.Monad.Reader as MTL
import Control.Monad.State.Strict (MonadState)

-- Streaming
import SupplyChain (Vendor (..), Factory, Supply ((:->)), (>->))
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

-- text :: c -> Move c m e ()
-- text = _

-- text :: Nontrivial xs x -> Move xs x e m ()
-- text = someOfNontrivialText A.>=> (maybe (return ()) (castTo @Any . text) . Nontrivial.refine)
--   where
--     someOfNontrivialText x = Action.Unsafe.AtomicMove $ case curse of
--         CursorRW{ init, input, commit } -> run $ Cursor.next input >>= \case
--             Nothing -> return (Left F.failure)
--             Just y ->
--                 if x `Nontrivial.isPrefixOf` y
--                 then commit (Nontrivial.length x) $> Right ListLike.empty
--                 else
--                 if y `Nontrivial.isPrefixOf` x
--                 then commit (Nontrivial.length y) $>
--                       Right
--                         (
--                           ListLike.drop
--                               (ListLike.length (Nontrivial.generalize y))
--                               (Nontrivial.generalize x)
--                         )
--                 else return (Left F.failure)

newtype Buffer c = Buffer{ bufferSeq :: Seq c }

data ViewBuffer c =
    Start -- ^ The unseen and unviewed buffers are the same
  | Unviewed (Buffer c) -- ^ The unviewed buffer, which may differ from the uncommitted buffer

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
        StepReset -> pure (() :-> go Start)
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
        x :<| xs -> pure (Just x :-> goUnviewed xs)
        Empty -> SupplyChain.order Stream.nextMaybe >>= \case
            Nothing -> pure (Nothing :-> goUnviewed Empty)
            Just x -> feedCommitBuffer x $> (Just x :-> goUnviewed Empty)
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
            DropAll -> setUncommittedChunks xs $> (AdvanceSuccess :-> go unviewed)
            DropPart{ dropRemainder = x' } -> setUncommittedChunks (x' :<| xs) $> (AdvanceSuccess :-> go unviewed)
            DropInsufficient{ dropShortfall = n' } -> handleCommit unviewed n' xs
        Empty -> SupplyChain.order Stream.nextMaybe >>= \case
            Nothing -> pure (YouCanNotAdvance{ shortfall = n } :-> go unviewed)
            Just x -> handleCommit unviewed' n (x :<| Empty)
              where
                unviewed' = case unviewed of
                    Start -> Unviewed (Buffer (x :<| Empty))
                    Unviewed (Buffer xs) -> Unviewed (Buffer (x :<| xs))
      where
        setUncommittedChunks :: Seq c -> Factory up action ()
        setUncommittedChunks xs = SupplyChain.perform $ assign buffer (Buffer xs)
