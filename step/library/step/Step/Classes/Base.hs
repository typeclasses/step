{-# language FlexibleContexts, FlexibleInstances, TypeFamilies, ConstraintKinds, KindSignatures #-}

module Step.Classes.Base where

import Step.Internal.Prelude

import Loc (Loc)

import Step.TakeOrLeave (TakeOrLeave (..))
import qualified Step.TakeOrLeave as TakeOrLeave

import qualified Monad

import qualified Text as T

class Monad m => Char1 m where

    type Text m :: Type

    considerChar :: ListLike (Text m) char =>
        (char -> TakeOrLeave b a)
            -- ^ Selection function, given the next a character as its argument;
            --   if 'Take', the cursor advances by 1;
            --   if 'Leave', the cursor will remain unmoved
        -> m (Maybe (TakeOrLeave b a))
            -- ^ The result of the selection function, or Nothing if end of input

    peekCharMaybe :: ListLike (Text m) char => m (Maybe char)
    peekCharMaybe = considerChar Leave <&> fmap TakeOrLeave.collapse

    takeCharMaybe :: ListLike (Text m) char => m (Maybe char)
    takeCharMaybe = considerChar (Take . Just) <&> Monad.join . fmap TakeOrLeave.collapse

    atEnd :: ListLike (Text m) char => m Bool
    atEnd = isNothing <$> peekCharMaybe

    {-# minimal considerChar #-}

class Monad m => Locating m where
    position :: m Loc

class Monad m => Fallible m where

    type Error m :: Type

    failure :: m (Error m)

class Char1 m => TakeAll m where
    -- | Consume the rest of the input
    takeAll :: ListLike (Text m) char => m (Text m)

class Char1 m => SkipTextNonAtomic m where
    skipTextNonAtomic :: ListLike (Text m) char => Eq char => Text m -> m Bool
        -- ^ Return value indicates whether operation succeeded

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
    peekCharMaybe = ReaderT \_ -> peekCharMaybe
    atEnd = ReaderT \_ -> atEnd
    considerChar f = ReaderT \_ -> considerChar f

instance TakeAll m => TakeAll (ReaderT r m) where
    takeAll = ReaderT \_ -> takeAll

instance SkipTextNonAtomic m => SkipTextNonAtomic (ReaderT r m) where
    skipTextNonAtomic x = ReaderT \_ -> skipTextNonAtomic x

instance Locating m => Locating (ReaderT r m) where
    position = ReaderT \_ -> position

instance Fallible m => Fallible (ReaderT r m) where
    type Error (ReaderT r m) = Error m
    failure = ReaderT \_ -> failure

instance Monad m => Configure (ReaderT r m) where
    type Config (ReaderT r m) = r
    configure = withReaderT
