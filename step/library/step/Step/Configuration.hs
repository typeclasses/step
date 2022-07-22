{-# language FlexibleContexts, FlexibleInstances #-}

module Step.Configuration where

import Step.Internal.Prelude

import Text (Text)

class Monad m => Configure m where
    type Config m :: Type
    configure :: (Config m -> Config m) -> m a -> m a

class HasContextStack config where
    contextStackLens :: Lens' config [Text]

instance Monad m => Configure (ReaderT r m) where
    type Config (ReaderT r m) = r
    configure = withReaderT
