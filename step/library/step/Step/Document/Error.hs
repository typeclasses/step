{-# language DerivingStrategies #-}

module Step.Document.Error where

import Step.Internal.Prelude

data Error = Error{ context :: [Text] }
    deriving stock (Eq, Show)
