{-# language DerivingStrategies #-}

module Step.Document.Config where

import Step.Internal.Prelude

import qualified Step.Classes as Class

data Config = Config{ context :: [Text] }
    deriving stock (Eq, Show)

instance Default Config
  where
    def = Config{ context = [] }

instance Class.HasContextStack Config where
    contextStackLens = lens context \x y -> x{ context = y }
