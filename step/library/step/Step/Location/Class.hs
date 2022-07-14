module Step.Location.Class where

import Step.Internal.Prelude

import Step.ActionTypes.Types

import qualified Step.ActionTypes.Unsafe as Action.Unsafe

import Loc (Loc)

class Monad m => Locating m where
    position :: m Loc

instance Locating m => Locating (ReaderT r m) where
    position = ReaderT \_ -> position
