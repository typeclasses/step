{-# language ExistentialQuantification, FlexibleContexts, FlexibleInstances, TypeFamilies, ConstraintKinds, KindSignatures #-}

module Step.Classes.Base where

import Step.Internal.Prelude hiding (while)

import Loc (Loc)

import Step.TakeOrLeave (TakeOrLeave (..))
import qualified Step.TakeOrLeave as TakeOrLeave

import qualified Monad

import qualified Text as T

import Step.Input.CursorPosition (CursorPosition)

class (ListLike (Text m) (Char m), Monad m) => Char1 m where

    type Text m :: Type
    type Char m :: Type

class Monad m => Counting m where
    cursorPosition :: m CursorPosition

class Monad m => Locating m where
    position :: m Loc

class Monad m => Fallible m where

    type Error m :: Type

    failure :: m (Error m)

class Monad m => Configure m where
    type Config m :: Type
    configure :: (Config m -> Config m) -> m a -> m a

class HasContextStack config where
    contextStackLens :: Lens' config [T.Text]

class Monad m => FillBuffer1 m where
    -- | Fill the buffer to at least one character, if possible
    fillBuffer1 :: m ()

class Monad m => BufferMore m where
    -- | Read one chunk of input, if possible
    bufferMore :: m ()


-- ReaderT instances

instance Char1 m => Char1 (ReaderT r m) where
    type Text (ReaderT r m) = Text m
    type Char (ReaderT r m) = Char m

instance Locating m => Locating (ReaderT r m) where
    position = lift position

instance Fallible m => Fallible (ReaderT r m) where
    type Error (ReaderT r m) = Error m
    failure = lift failure

instance Char1 m => Configure (ReaderT r m) where
    type Config (ReaderT r m) = r
    configure = withReaderT

instance (Monad m, Counting m) => Counting (ReaderT r m) where
    cursorPosition = lift cursorPosition
