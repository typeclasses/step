{-# language ExistentialQuantification, FlexibleContexts, FlexibleInstances, TypeFamilies, ConstraintKinds, KindSignatures #-}

module Step.Classes.Base where

import Step.Internal.Prelude hiding (while)

import Loc (Loc)

import Step.TakeOrLeave (TakeOrLeave (..))
import qualified Step.TakeOrLeave as TakeOrLeave

import qualified Monad

import qualified Text as T

import Step.Input.CursorPosition (CursorPosition)

data Consideration1 text b a =
    forall char. ListLike text char =>
        Consideration1 (char -> TakeOrLeave b a)

class (ListLike (Text m) (Char m), Monad m) => Char1 m where

    type Text m :: Type
    type Char m :: Type

    considerChar ::
        Consideration1 (Text m) b a
            -- ^ Selection function, given the next a character as its argument;
            --   if 'Take', the cursor advances by 1;
            --   if 'Leave', the cursor will remain unmoved
        -> m (Maybe (TakeOrLeave b a))
            -- ^ The result of the selection function, or Nothing if end of input

    peekCharMaybe :: m (Maybe (Char m))
    peekCharMaybe = considerChar (Consideration1 Leave) <&> fmap TakeOrLeave.collapse

    takeCharMaybe :: m (Maybe (Char m))
    takeCharMaybe = considerChar (Consideration1 (Take . Just)) <&> Monad.join . fmap TakeOrLeave.collapse

    atEnd :: m Bool
    atEnd = isNothing <$> peekCharMaybe

    {-# minimal considerChar #-}

class Monad m => Counting m where
    cursorPosition :: m CursorPosition

class Char1 m => Locating m where
    position :: m Loc

class Char1 m => Fallible m where

    type Error m :: Type

    failure :: m (Error m)

class Char1 m => TakeAll m where
    -- | Consume the rest of the input
    takeAll :: m (Text m)

class Char1 m => Configure m where
    type Config m :: Type
    configure :: (Config m -> Config m) -> m a -> m a

class HasContextStack config where
    contextStackLens :: Lens' config [T.Text]

class Char1 m => FillBuffer1 m where
    -- | Fill the buffer to at least one character, if possible
    fillBuffer1 :: m ()

class Char1 m => BufferMore m where
    -- | Read one chunk of input, if possible
    bufferMore :: m ()


-- ReaderT instances

instance Char1 m => Char1 (ReaderT r m) where
    type Text (ReaderT r m) = Text m
    type Char (ReaderT r m) = Char m
    peekCharMaybe = lift peekCharMaybe
    atEnd = lift atEnd
    considerChar f = lift (considerChar f)

instance TakeAll m => TakeAll (ReaderT r m) where
    takeAll = lift takeAll

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
