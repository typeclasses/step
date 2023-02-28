module Miscellany where

import Essentials

maybeAlternative :: Monad m => m (Maybe a) -> m (Maybe a) -> m (Maybe a)
maybeAlternative a b = a >>= \case
    Just x -> pure (Just x)
    Nothing -> b
