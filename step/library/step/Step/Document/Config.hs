{-# language DerivingStrategies, TemplateHaskell #-}

module Step.Document.Config where

import Step.Internal.Prelude

data Config text = Config{ context :: [text] }
    deriving stock (Eq, Show)

makeLensesFor [("context", "contextLens")] ''Config

instance Default (Config text)
  where
    def = Config{ context = [] }
