{-# language FlexibleContexts, FlexibleInstances, TypeFamilies, ConstraintKinds, KindSignatures #-}

module Step.LookAhead.Class where

import Step.Internal.Prelude hiding (Text)

import Step.ActionTypes

class Monad m => LookAhead m where

    type Text m :: Type

    next :: ListLike (Text m) char => m (Maybe char)

    atEnd :: ListLike (Text m) char => m Bool
    atEnd = isNothing <$> next

type Char base char = (ListLike (Text base) char) :: Constraint

instance LookAhead m => LookAhead (ReaderT r m) where
    type Text (ReaderT r m) = Text m
    next = ReaderT \_ -> next
    atEnd = ReaderT \_ -> atEnd
