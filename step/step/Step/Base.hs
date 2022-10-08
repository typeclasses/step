module Step.Base where

import Step.Action.Core
import Step.Chunk.Core

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
import SupplyChain (Vendor (..), Client, Supply ((:->)))
import SupplyChain.Interface.TerminableStream (TerminableStream)
import qualified SupplyChain
import qualified SupplyChain.Interface.TerminableStream as Stream

one :: Positive Natural
one = PositiveUnsafe 1

reset :: forall c es e. SureQuery c es e ()
reset = SureQuery $ SupplyChain.order StepReset

commit :: forall c es e. Positive Natural -> AtomicMove c es e AdvanceResult
commit n = AtomicMove $ Atom $ Query $ return $ Sure $ SupplyChain.order $ StepCommit n

fail :: MonadReader e m => Failure c m e a
fail = Failure MTL.ask

-- | Like 'nextMaybe', but doesn't reset first
nextMaybe' :: forall c m e. SureQuery c m e (Maybe c)
nextMaybe' = SureQuery $ SupplyChain.order StepNext

nextMaybe :: SureQuery c m e (Maybe c)
nextMaybe = reset `bindAction` \() -> nextMaybe'

nextCharMaybe :: Chunk c => SureQuery c m e (Maybe (OneOf c))
nextCharMaybe = nextMaybe <&> fmap @Maybe (popItem . leftView)

nextChar :: Chunk c => MonadReader e m => Query c m e (OneOf c)
nextChar = nextCharMaybe `bindAction` maybe (castTo @Query fail) return

takeChar :: Chunk c => MonadReader e m => AtomicMove c m e (OneOf c)
takeChar = nextChar `bindAction` \x -> commit one $> x

takeCharMaybe :: Chunk c => MonadReader e m => Sure c m e (Either e (OneOf c))
takeCharMaybe = try takeChar

next :: MonadReader e m => Query c m e c
next = nextMaybe `bindAction` maybe (castTo @Query fail) return

-- | Like 'next', but doesn't reset first
next' :: MonadReader e m => Query c m e c
next' = nextMaybe' `bindAction` maybe (castTo @Query fail) return

takeNext :: forall c e m. Chunk c => MonadReader e m => AtomicMove c m e c
takeNext = next `bindAction` \c -> commit (length @c c) $> c

takeNextMaybe :: Chunk c => MonadReader e m => Sure c m e (Either e c)
takeNextMaybe = try takeNext

satisfyJust :: Chunk c => MonadReader e m => (OneOf c -> Maybe a) -> AtomicMove c m e a
satisfyJust ok = nextCharMaybe `bindAction` \x -> case x >>= ok of Nothing -> castTo fail; Just y -> commit one $> y

skip :: forall c e m. Chunk c => MonadReader e m => Positive Natural -> Move c m e AdvanceResult
skip n = next `bindAction` \x ->
    case Positive.minus (length @c x) n of
        Signed.Minus n' ->
            commit (length @c x) `bindAction` \_ -> skip n'
        _ -> castTo @Move (commit n)

skip0 :: forall c e m. Chunk c => MonadReader e m => Natural -> Any c m e AdvanceResult
skip0 = maybe (return AdvanceSuccess) (castTo @Any . skip)  . preview Positive.refine

ensureAtLeast :: forall c e m. Chunk c => MonadReader e m => Positive Natural -> Query c m e ()
ensureAtLeast = \n -> castTo @Query reset `bindAction` \() -> go n
  where
    go :: MonadReader e m => Positive Natural -> Query c m e ()
    go n = next' `bindAction` \x ->
        case Positive.minus n (length @c x) of
            Signed.Plus n' -> go n'
            _ -> return ()

skipAtomically :: forall c e m. Chunk c => MonadReader e m => Positive Natural -> AtomicMove c m e AdvanceResult
skipAtomically n = ensureAtLeast n `bindAction` \() -> commit n

skipAtomically0 :: forall c e m. Chunk c => MonadReader e m => Natural -> Atom c m e AdvanceResult
skipAtomically0 = maybe (trivial AdvanceSuccess) (castTo @Atom . skipAtomically)  . preview Positive.refine

atEnd :: SureQuery c es e Bool
atEnd = reset `bindAction` \() -> nextMaybe' <&> isNothing

end :: MonadReader e m => Query c m e ()
end = atEnd `bindAction` \e -> if e then trivial () else castTo @Query fail

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

bufferedStepper :: forall s action c. Chunk c => MonadState s action =>
    Lens' s (Buffer c) -> Vendor (TerminableStream c) (Step 'RW c) action
bufferedStepper buffer = go Start
  where
    go :: ViewBuffer c -> Vendor (TerminableStream c) (Step 'RW c) action
    go unviewed = Vendor \case
        StepReset -> pure (() :-> go Start)
        StepNext -> getUnviewedChunks >>= handleNext
        StepCommit n -> getUncommittedChunks >>= handleCommit unviewed n
      where
        getUncommittedChunks :: Client (TerminableStream c) action (Seq c)
        getUncommittedChunks = bufferSeq <$> SupplyChain.perform (use buffer)

        getUnviewedChunks :: Client (TerminableStream c) action (Seq c)
        getUnviewedChunks = case unviewed of
            Unviewed b -> pure (bufferSeq b)
            Start -> getUncommittedChunks

    handleNext ::
        Seq c -- unviewed chunks
        -> Client (TerminableStream c) action
              (Supply (TerminableStream c) (Step 'RW c) action (Maybe c))
    handleNext = \case
        x :<| xs -> pure (Just x :-> goUnviewed xs)
        Empty -> SupplyChain.order Stream.NextMaybe >>= \case
            Nothing -> pure (Nothing :-> goUnviewed Empty)
            Just x -> feedCommitBuffer x $> (Just x :-> goUnviewed Empty)
      where
        goUnviewed :: Seq c -> Vendor (TerminableStream c) (Step 'RW c) action
        goUnviewed unviewed = go (Unviewed (Buffer unviewed))

        feedCommitBuffer :: c -> Client (TerminableStream c) action ()
        feedCommitBuffer x = SupplyChain.perform $
            modifying buffer \(Buffer xs) -> Buffer (xs :|> x)

    handleCommit ::
        ViewBuffer c
        -> Positive Natural -- how much to commit
        -> Seq c -- uncommitted chunks
        -> Client (TerminableStream c) action
              (Supply (TerminableStream c) (Step 'RW c) action AdvanceResult)
    handleCommit unviewed n = \case
        x :<| xs -> case drop n x of
            DropAll -> setUncommittedChunks xs $> (AdvanceSuccess :-> go unviewed)
            DropPart{ dropRemainder = x' } -> returnToCommitBuffer x' $> (AdvanceSuccess :-> go unviewed)
            DropInsufficient{ dropShortfall = n' } -> handleCommit unviewed n' xs
        Empty -> SupplyChain.order Stream.NextMaybe >>= \case
            Nothing -> pure (YouCanNotAdvance{ shortfall = n } :-> go unviewed)
            Just x -> handleCommit unviewed' n (x :<| Empty)
              where
                unviewed' = case unviewed of
                    Start -> Unviewed (Buffer (x :<| Empty))
                    Unviewed (Buffer xs) -> Unviewed (Buffer (x :<| xs))
      where
        setUncommittedChunks :: Seq c -> Client (TerminableStream c) action ()
        setUncommittedChunks xs = SupplyChain.perform $ assign buffer (Buffer xs)

        returnToCommitBuffer :: c -> Client (TerminableStream c) action ()
        returnToCommitBuffer x = SupplyChain.perform $
            modifying buffer \(Buffer xs) -> Buffer (x :<| xs)
