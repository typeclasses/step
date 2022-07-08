{-# language DerivingStrategies #-}

module Step.Document.Config where

import Step.Internal.Prelude

data Config text = Config{ context :: [text] }
    deriving stock (Eq, Show)

contextLens :: Lens' (Config text) [text]
contextLens = lens context \x y -> x{ context = y }

instance Default (Config text)
  where
    def = Config{ context = [] }
