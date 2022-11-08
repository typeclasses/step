module Step.Interface.Buffer
  (
    Buffer (..), bufferedStepper, pureStepper,
    doubleBuffer, privateDoubleBuffer,
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
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

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

pattern One :: a -> Buffer a
pattern One x = Buffer (x Seq.:<| Seq.Empty)

pattern (:<) :: c -> Buffer c -> Buffer c
pattern x :< xs <- Buffer (x Seq.:<| (Buffer -> xs))
  where
    x :< Buffer xs = Buffer (x Seq.:<| xs)

pattern (:>) :: Buffer c -> c -> Buffer c
pattern xs :> x <- Buffer ((Buffer -> xs) Seq.:|> x)
  where
    Buffer xs :> x = Buffer (xs Seq.:|> x)

pattern Empty :: Buffer a
pattern Empty = Buffer Seq.Empty

{-# complete Empty, (:>) #-}
{-# complete Empty, (:<) #-}

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
bufferedStepper buffer = Vendor \request -> do
    b <- SupplyChain.perform $ use buffer
    handle (doubleBuffer report b) request
  where
    report b = SupplyChain.perform (assign buffer b)

data DoubleBuffer c = DoubleBuffer{ dbCommit :: Buffer c, dbView :: Buffer c }

privateDoubleBuffer :: forall c up action. Chunk c => IsTerminableStream c up =>
    Vendor up (CommittableChunkStream c) action
privateDoubleBuffer = doubleBuffer (\_ -> pure ()) Empty

doubleBuffer :: forall c up action. Chunk c => IsTerminableStream c up =>
    (Buffer c -> Job up action ()) ->
    Buffer c -> Vendor up (CommittableChunkStream c) action
doubleBuffer report b = go (DoubleBuffer b b)
  where
    go :: Chunk c => IsTerminableStream c up =>
        DoubleBuffer c -> Vendor up (CommittableChunkStream c) action
    go s = Vendor \case
        I.Reset -> handleReset go s
        I.NextMaybe -> handleNext report go s
        I.Commit n -> handleCommit report go s n

handleReset :: (DoubleBuffer c -> Vendor up down action) -> DoubleBuffer c
    -> Job up action (Referral up down action ())
handleReset go s = pure $ Referral () $ go s{ dbView = dbCommit s }

handleNext :: IsTerminableStream c up =>
    (Buffer c -> Job up action ()) -> (DoubleBuffer c -> Vendor up down action)
    -> DoubleBuffer c -> Job up action (Referral up down action (Maybe c))
handleNext report go s =  case dbView s of
    x :< xs -> pure $ Referral (Just x) $ go s{ dbView = xs }
    Empty -> SupplyChain.order Stream.nextMaybe >>= \case
        Nothing -> pure $ Referral Nothing $ go s
        Just x -> report (dbCommit s :> x) $> Referral (Just x) (go s{ dbCommit = dbCommit s :> x })

handleCommit :: Chunk c => IsTerminableStream c up =>
    (Buffer c -> Job up action ())
    -> (DoubleBuffer c -> Vendor up (CommittableChunkStream c) action)
    -> DoubleBuffer c -> Positive Natural
    -> Job up action (Referral up (CommittableChunkStream c) action AdvanceResult)
handleCommit report go s n = case dbCommit s of
      x :< xs -> case drop n x of
          DropAll -> report xs $> Referral AdvanceSuccess (go s{ dbCommit = xs })
          DropPart{ dropRemainder = x' } -> report (x' :< xs) $> Referral AdvanceSuccess (go s{ dbCommit = x' :< xs })
          DropInsufficient{ dropShortfall = n' } -> report xs *> (go s{ dbCommit = xs } `handle` I.Commit n')
      Empty -> SupplyChain.order Stream.nextMaybe >>= \case
          Nothing -> pure $ Referral YouCanNotAdvance{ shortfall = n } $ go s
          Just x -> report (One x) *> (go s{ dbCommit = One x, dbView = dbView s :> x } `handle` I.Commit n)
