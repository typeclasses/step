{-# language DerivingStrategies #-}

module Step.Document.Config where

import Step.Internal.Prelude

data Config = Config{ context :: [Text] }
    deriving stock (Eq, Show)

contextLens :: Lens' Config [Text]
contextLens = lens context \x y -> x{ context = y }

instance Default Config
  where
    def = Config{ context = [] }
