module Step.Util.Prelude (module X, mtimes) where

import BasePrelude as X (Bool (..), Semigroup ((<>)), Monoid (mempty), Ordering (..), Maybe (..), Either (..), Eq, (==), (/=), Ord (compare), Show, Num, (+), (-), Integer, Int, fromIntegral, ($), (.), (<$>), error, id, Monad, (>>=), (=<<), return, Char, elem, (&&), (||), (<), (>), (<=), (>=), Functor, Applicative, fmap, IO, (<*), (*>), (<*>), Integral)
import Applicative as X ((<|>))
import Foldable as X (asum)
import Function as X (fix)
import ListLike as X (ListLike)
import MonadTrans as X (lift)
import Functor as X ((<&>))
import Identity as X (Identity (Identity), runIdentity)
import Map as X (Map)
import Maybe as X (fromMaybe, isJust)
import Natural as X (Natural)
import Monad as X (replicateM)
import Seq as X (Seq)
import Set as X (Set)
import State as X (evalStateT, execStateT, StateT(..), put, get, modify')
import Reader as X (ReaderT (ReaderT))
import Except as X (ExceptT (ExceptT))
import String as X (IsString)
import Monoid as X (Endo (Endo), appEndo)

import qualified Semigroup

mtimes :: Integral n => Monoid a => n -> a -> a
mtimes = Semigroup.mtimesDefault
