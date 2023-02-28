module Cursor.Morph.Examples.DecodeASCII
  (
    {- * Transformations -} decodeAscii,
  )
  where

import Essentials
import Cursor.Morph.Type

import Block (ASCII1, ByteString1, End (..), Take (..), Shortfall (..))
import Cursor.Advancement (Advancement (..), commitAlternative)
import Cursor.Buffer (Buffer)
import Cursor.Interface (Mode (..), Cursor (..), IsCursor, Step (..))
import Control.Monad.State (StateT)
import Control.Monad.Trans (lift)
import Data.Function (flip)
import Data.Sequence (Seq (..))
import Data.Word (Word8)
import Integer (Positive)
import Miscellany (maybeAlternative)
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
decodeAscii = decodeAscii' (S Buffer.empty Alive)

decodeAscii' :: S -> MorphPlus up action 'Write Word8 ByteString1 ASCII.Char ASCII1
decodeAscii' s = Vendor \case
    Next -> next s
    Commit n -> commit s n
    Reset -> reset s
    Flush -> reset s

next :: IsCursor 'Write ByteString1 up =>
    S -> Job up action (Referral up (Cursor 'Write ASCII1) action (Step ASCII1))
next s = do
    (xm, s') <- flip State.runStateT s $
        maybeAlternative (Optics.zoom buffer Buffer.next) nextFromUpstream
    pure $ Referral (maybe End Item xm) (decodeAscii' s')

nextFromUpstream :: IsCursor 'Write ByteString1 up =>
    StateT S (Job up action) (Maybe ASCII1)
nextFromUpstream = Optics.use vitality >>= \case
    Dead -> pure Nothing
    Alive -> lift Cursor.next >>= \case
        End -> pure Nothing
        Item x -> case Block.spanAscii Front x of
            Nothing -> Optics.assign vitality Dead $> Nothing
            Just (a, remainder) -> do
                modifying uncommitted (:|> a)
                Monad.when (Maybe.isJust remainder) $ Optics.assign vitality Dead
                pure $ Just a

commit :: IsCursor 'Write ByteString1 up => S -> Positive
    -> Job up action (Referral up (Cursor 'Write ASCII1) action (Advancement () ()))
commit s n = do
    (r, s') <- flip State.runStateT s $
        commitAlternative (Optics.zoom buffer . Buffer.commit) commitFromUpstream n
    pure $ Referral r (decodeAscii' s')

commitFromUpstream :: IsCursor 'Write ByteString1 up =>
    Positive -> StateT S (Job up action) (Advancement () ())
commitFromUpstream n = Optics.use vitality >>= \case
    Dead -> pure $ YouCanNotAdvance (Shortfall n) ()
    Alive -> lift Cursor.next >>= \case
        End -> do
            Optics.assign vitality Dead
            pure $ YouCanNotAdvance (Shortfall n) ()
        Item x -> case Block.spanAscii Front x of
            Nothing -> do
                Optics.assign vitality Dead
                pure $ YouCanNotAdvance (Shortfall n) ()
            Just (a, remainder) -> do
                Optics.modifying unviewed (:|> a)
                Optics.modifying uncommitted (:|> a)
                Monad.when (Maybe.isJust remainder) $ Optics.assign vitality Dead
                case Block.take Front n a of
                    TakeAll -> pure $ AdvanceSuccess ()
                    TakePart{ takeRemainder = x' } -> do
                        Optics.assign uncommitted (Seq.singleton x')
                        pure $ AdvanceSuccess ()
                    TakeInsufficient (Shortfall n') -> commitFromUpstream n'

reset :: IsCursor 'Write ByteString1 up => S
    -> Job up action (Referral up (Cursor 'Write ASCII1) action ())
reset s = do
    let s' = s & Optics.over buffer \b ->
                  b{ Buffer.bufferUnviewed = Buffer.bufferUncommitted b }
    pure $ Referral () (decodeAscii' s')
