module Cursor.Buffer where

import Essentials

import Block (Block, Take (..), take, End (..), Shortfall (..))
import Control.Monad.State (StateT)
import Cursor.Advancement (Advancement (..))
import Data.Sequence (Seq (..))
import Integer (Positive)
import Optics (assign, use)
import SupplyChain (Job)

import qualified Data.Sequence as Seq
import qualified Optics

data Buffer block = Buffer
    { bufferUncommitted :: Seq block -- ^ 'uncommitted'
    , bufferUnviewed    :: Seq block -- ^ 'unviewed'
    }

type BufferLens block =
    Optics.Lens (Buffer block) (Buffer block) (Seq block) (Seq block)

empty :: Buffer block
empty = Buffer mempty mempty

{-| Input that has been obtained from upstream but not committed -}
uncommitted :: BufferLens block

{-| Input that has been obtained from upstream but not viewed since the last reset -}
unviewed :: BufferLens block

uncommitted = Optics.lens bufferUncommitted \s a -> s{ bufferUncommitted = a }
unviewed    = Optics.lens bufferUnviewed    \s a -> s{ bufferUnviewed    = a }

{-| Tries to pop the head off of the 'unviewed' buffer -}
next :: StateT (Buffer block) (Job up action) (Maybe block)
next = use unviewed >>= \case
    Seq.Empty -> pure Nothing
    x :<| xs -> assign unviewed xs $> Just x

{-| Tries to pop some fixed number of items from the 'uncommitted' buffer -}
commit :: Block item block =>
    Positive -- ^ How many items to commit
    -> StateT (Buffer block) (Job up action) (Advancement () ())
commit n = use uncommitted >>= \case
    Seq.Empty -> pure $ YouCanNotAdvance (Shortfall n) ()
    x :<| xs -> case take Front n x of
        TakeAll -> assign uncommitted xs $> AdvanceSuccess ()
        TakePart{ takeRemainder = x' } -> assign uncommitted (x' :<| xs) $> AdvanceSuccess ()
        TakeInsufficient (Shortfall n') -> assign uncommitted xs *> commit n'
