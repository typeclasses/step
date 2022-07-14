{-# language FlexibleContexts, FlexibleInstances, TypeFamilies, ConstraintKinds, KindSignatures #-}

module Step.Classes where

import Step.Internal.Prelude hiding (Text)

import Loc (Loc)

import Step.TakeOrLeave (TakeOrLeave (..))
import qualified Step.TakeOrLeave as TakeOrLeave

import qualified Monad

class Monad m => Peek1 m where

    type Text m :: Type

    next :: PeekChar m char => m (Maybe char)

    atEnd :: PeekChar m char => m Bool
    atEnd = isNothing <$> next

    {-# minimal next #-}

class Peek1 m => Take1 m where
    considerChar :: PeekChar m char =>
        (char -> TakeOrLeave b a)
            -- ^ Selection function, given the next a character as its argument;
            --   if 'Take', the cursor advances by 1;
            --   if 'Leave', the cursor will remain unmoved
        -> m (Maybe (TakeOrLeave b a))
            -- ^ The result of the selection function, or Nothing if end of input

    peekCharMaybe :: PeekChar m char => m (Maybe char)
    peekCharMaybe = considerChar (Leave . Just) <&> Monad.join . fmap TakeOrLeave.collapse

    takeCharMaybe :: PeekChar m char => m (Maybe char)
    takeCharMaybe = considerChar (Take . Just) <&> Monad.join . fmap TakeOrLeave.collapse

    {-# minimal considerChar #-}

class Monad m => Locating m where
    position :: m Loc

class Monad m => Fallible m where

    type Error m :: Type

    failure :: m (Error m)


-- Aliases

type PeekChar m char = (Peek1 m, ListLike (Text m) char) :: Constraint

type TakeChar m char = (Take1 m, ListLike (Text m) char) :: Constraint


-- ReaderT instances

instance Peek1 m => Peek1 (ReaderT r m) where
    type Text (ReaderT r m) = Text m
    next = ReaderT \_ -> next
    atEnd = ReaderT \_ -> atEnd

instance Take1 m => Take1 (ReaderT r m) where
    considerChar f = ReaderT \_ -> considerChar f

instance Locating m => Locating (ReaderT r m) where
    position = ReaderT \_ -> position

instance Fallible m => Fallible (ReaderT r m) where
    type Error (ReaderT r m) = Error m
    failure = ReaderT \_ -> failure
