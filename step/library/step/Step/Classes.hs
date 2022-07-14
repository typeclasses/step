{-# language FlexibleContexts, FlexibleInstances, TypeFamilies, ConstraintKinds, KindSignatures #-}

module Step.Classes where

import Step.Internal.Prelude hiding (Text)

import Loc (Loc)

import Step.TakeOrLeave (TakeOrLeave (..))
import qualified Step.TakeOrLeave as TakeOrLeave

import qualified Monad

import qualified Text as T

class Monad m => Peek1 m where

    type Text m :: Type

    peekCharMaybe :: ListLike (Text m) char => m (Maybe char)

    atEnd :: ListLike (Text m) char => m Bool
    atEnd = isNothing <$> peekCharMaybe

    {-# minimal peekCharMaybe #-}

class Peek1 m => Take1 m where
    considerChar :: ListLike (Text m) char =>
        (char -> TakeOrLeave b a)
            -- ^ Selection function, given the next a character as its argument;
            --   if 'Take', the cursor advances by 1;
            --   if 'Leave', the cursor will remain unmoved
        -> m (Maybe (TakeOrLeave b a))
            -- ^ The result of the selection function, or Nothing if end of input

    takeCharMaybe :: ListLike (Text m) char => m (Maybe char)
    takeCharMaybe = considerChar (Take . Just) <&> Monad.join . fmap TakeOrLeave.collapse

    {-# minimal considerChar #-}

class Monad m => Locating m where
    position :: m Loc

class Monad m => Fallible m where

    type Error m :: Type

    failure :: m (Error m)

class Peek1 m => TakeAll m where
    -- | Consume the rest of the input
    takeAll :: ListLike (Text m) char => m (Text m)

class Monad m => Configure m where
    type Config m :: Type
    configure :: (Config m -> Config m) -> m a -> m a

class HasContextStack config where
    contextStackLens :: Lens' config [T.Text]


-- ReaderT instances

instance Peek1 m => Peek1 (ReaderT r m) where
    type Text (ReaderT r m) = Text m
    peekCharMaybe = ReaderT \_ -> peekCharMaybe
    atEnd = ReaderT \_ -> atEnd

instance Take1 m => Take1 (ReaderT r m) where
    considerChar f = ReaderT \_ -> considerChar f

instance TakeAll m => TakeAll (ReaderT r m) where
    takeAll = ReaderT \_ -> takeAll

instance Locating m => Locating (ReaderT r m) where
    position = ReaderT \_ -> position

instance Fallible m => Fallible (ReaderT r m) where
    type Error (ReaderT r m) = Error m
    failure = ReaderT \_ -> failure

instance Monad m => Configure (ReaderT r m) where
    type Config (ReaderT r m) = r
    configure = withReaderT
