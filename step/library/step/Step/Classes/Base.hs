{-# language ExistentialQuantification, FlexibleContexts, FlexibleInstances, TypeFamilies, ConstraintKinds, KindSignatures #-}

module Step.Classes.Base where

import Step.Internal.Prelude hiding (while)

import Loc (Loc)

import qualified Monad

import qualified Text as T

import Step.Input.CursorPosition (CursorPosition)

class Monad m => Fallible m where

    type Error m :: Type

    failure :: m (Error m)

class Monad m => Configure m where
    type Config m :: Type
    configure :: (Config m -> Config m) -> m a -> m a

class HasContextStack config where
    contextStackLens :: Lens' config [T.Text]


-- ReaderT instances

instance Fallible m => Fallible (ReaderT r m) where
    type Error (ReaderT r m) = Error m
    failure = lift failure

instance Monad m => Configure (ReaderT r m) where
    type Config (ReaderT r m) = r
    configure = withReaderT
