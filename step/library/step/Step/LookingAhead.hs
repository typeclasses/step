{-# language TypeFamilies #-}

module Step.LookingAhead where

import Step.Internal.Prelude

import Step.Nontrivial.Base

class Monad m => Prophetic m where
    type Text m :: Type
    type Char m :: Type
    forecast :: ListT m (Nontrivial (Text m) (Char m))

instance Prophetic m => Prophetic (ReaderT r m) where
    type Text (ReaderT r m) = Text m
    type Char (ReaderT r m) = Char m
    forecast = changeBaseListT lift forecast
