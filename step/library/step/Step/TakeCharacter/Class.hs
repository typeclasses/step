{-# language FlexibleContexts, FlexibleInstances, TypeFamilies, ConstraintKinds, KindSignatures #-}

module Step.TakeCharacter.Class where

import Step.Internal.Prelude

import Step.LookAhead.Class (LookAhead)
import qualified Step.LookAhead.Class as LookAhead

class LookAhead m => TakeCharacter m where
    takeCharMaybe :: LookAhead.Char m char => (char -> Maybe a) -> m (Maybe a)

instance TakeCharacter m => TakeCharacter (ReaderT r m) where
    takeCharMaybe f = ReaderT \_ -> takeCharMaybe f
