module Step.Document.Locating where

import Step.Internal.Prelude

import Loc (Loc)

class Locating m where
    position :: m Loc

instance (Monad m, Locating m) => Locating (ReaderT r m) where
    position = lift position
