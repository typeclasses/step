module Miscellany where

import Essentials

import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Data.Either (Either (..))

import qualified Control.Monad.State as MTL
import qualified Block

maybeAlternative :: Monad m => m (Maybe a) -> m (Maybe a) -> m (Maybe a)
maybeAlternative a b = a >>= \case
    Just x -> pure (Just x)
    Nothing -> b

overExcept :: Functor m => (e -> e') -> ExceptT e m a -> ExceptT e' m a
overExcept f ex = ExceptT $
    runExceptT ex <&> \case{ Left e -> Left (f e); Right x -> Right x }

mtlToBlockState :: MTL.State s a -> Block.State s a
mtlToBlockState (MTL.StateT f) = Block.State $
    f >>> runIdentity >>> (\(a, s) -> Block.StateResult a s)

blockToMtlState :: Block.State s a -> MTL.State s a
blockToMtlState (Block.State f) = MTL.StateT $
    f >>> (\(Block.StateResult a s) -> (a, s)) >>> Identity
