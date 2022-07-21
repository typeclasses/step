{-# language TypeFamilies #-}

module Step.Input.Cursor where

import Step.Internal.Prelude

import Step.Nontrivial.Base

import Step.Input.AdvanceResult

class Cursor m where
    type Text m :: Type
    type Char m :: Type
    advance :: Positive Natural -> m AdvanceResult
    forecast :: ListT m (Nontrivial (Text m) (Char m))

instance (Monad m, Cursor m) => Cursor (ReaderT r m) where
    type Text (ReaderT r m) = Text m
    type Char (ReaderT r m) = Char m
    advance n = lift (advance n)
    forecast = changeBaseListT lift forecast
