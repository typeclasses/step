{-# language FlexibleContexts, FlexibleInstances, TypeFamilies, ConstraintKinds, KindSignatures #-}

module Step.Classes where

import Step.Internal.Prelude hiding (Text)

import Loc (Loc)

class Monad m => Peek1 m where

    type Text m :: Type

    next :: PeekChar m char => m (Maybe char)

    atEnd :: PeekChar m char => m Bool
    atEnd = isNothing <$> next

type PeekChar m char = (Peek1 m, ListLike (Text m) char) :: Constraint

class Peek1 m => Take1 m where
    takeCharMaybe :: PeekChar m char => (char -> Maybe a) -> m (Maybe a)

class Monad m => Locating m where
    position :: m Loc


-- ReaderT instances

instance Peek1 m => Peek1 (ReaderT r m) where
    type Text (ReaderT r m) = Text m
    next = ReaderT \_ -> next
    atEnd = ReaderT \_ -> atEnd

instance Take1 m => Take1 (ReaderT r m) where
    takeCharMaybe f = ReaderT \_ -> takeCharMaybe f

instance Locating m => Locating (ReaderT r m) where
    position = ReaderT \_ -> position
