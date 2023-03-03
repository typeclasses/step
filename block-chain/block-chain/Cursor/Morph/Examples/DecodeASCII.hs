module Cursor.Morph.Examples.DecodeASCII
  (
    {- * Transformations -} decodeAscii,
  )
  where

import Essentials
import Cursor.Morph.Type

import Block (ASCII1, ByteString1, End (..), Take (..), Shortfall (..))
import Cursor.Advancement (Advancement (..))
import Cursor.Buffer (Buffer)
import Cursor.Interface (Mode (..), Cursor (..), IsCursor, Step (..))
import Control.Monad.State (StateT)
import Control.Monad.Trans (lift)
import Data.Sequence (Seq (..))
import Data.Word (Word8)
import Integer (Positive)
import Optics ((%), modifying)
import SupplyChain (Vendor (Vendor), Job, Referral (Referral))

import qualified ASCII.Char as ASCII
import qualified Block
import qualified Control.Monad as Monad
import qualified Control.Monad.State as State
import qualified Cursor.Buffer as Buffer
import qualified Cursor.Interface.Orders as Cursor
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Optics
import qualified SupplyChain.Vendor as Vendor

data Vitality = Dead | Alive

data S = S{ sBuffer :: Buffer ASCII1, sVitality :: Vitality }

buffer      :: Optics.Lens S S (Buffer ASCII1) (Buffer ASCII1)
vitality    :: Optics.Lens S S Vitality Vitality
uncommitted :: Optics.Lens S S (Seq ASCII1) (Seq ASCII1)
unviewed    :: Optics.Lens S S (Seq ASCII1) (Seq ASCII1)

buffer      = Optics.lens sBuffer   \s a -> s{ sBuffer = a }
vitality    = Optics.lens sVitality \s a -> s{ sVitality = a }
uncommitted = buffer % Buffer.uncommitted
unviewed    = buffer % Buffer.unviewed

decodeAscii :: MorphPlus up action 'Write Word8 ByteString1 ASCII.Char ASCII1
decodeAscii = Vendor.state (S Buffer.empty Alive) \case
    Next     -> State.runStateT $ next
    Commit n -> State.runStateT $ commit n
    Reset    -> State.runStateT $ reset
    Flush    -> State.runStateT $ reset

next :: IsCursor 'Write ByteString1 up => StateT S (Job up action) (Step ASCII1)
next = Optics.use unviewed >>= \case
    (Seq.:<|) x xs -> do
        Optics.assign unviewed xs
        pure $ Item x
    Seq.Empty -> Optics.use vitality >>= \case
        Dead -> pure End
        Alive -> do
            bufferMore
            next

commit :: IsCursor 'Write ByteString1 up => Positive
    -> StateT S (Job up action) (Advancement () ())
commit n = Optics.use uncommitted >>= \case
    Seq.Empty -> Optics.use vitality >>= \case
        Dead -> pure $ YouCanNotAdvance (Shortfall n) ()
        Alive -> do
            bufferMore
            commit n
    (Seq.:<|) x xs -> case Block.take Front n x of
        TakeAll -> do
            Optics.assign uncommitted xs
            _ <- lift $ Cursor.commitPositive n
            pure $ AdvanceSuccess ()
        TakeInsufficient (Shortfall n') -> do
            Optics.assign uncommitted xs
            _ <- lift $ Cursor.commitPositive $ Block.length x
            commit n'
        TakePart{ takeRemainder = x' } -> do
            Optics.assign uncommitted ((Seq.:<|) x' xs)
            _ <- lift $ Cursor.commitPositive n
            pure $ AdvanceSuccess ()

bufferMore :: IsCursor 'Write ByteString1 up => StateT S (Job up action) ()
bufferMore = lift Cursor.next >>= \case
    End -> Optics.assign vitality Dead
    Item x -> case Block.spanAscii Front x of
        Nothing -> Optics.assign vitality Dead
        Just (a, remainder) -> do
            Optics.modifying unviewed (:|> a)
            Optics.modifying uncommitted (:|> a)
            Monad.when (Maybe.isJust remainder) $ Optics.assign vitality Dead

reset :: IsCursor 'Write ByteString1 up => StateT S (Job up action) ()
reset = Optics.use uncommitted >>= Optics.assign unviewed
