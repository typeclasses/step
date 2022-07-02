module Step.Document.Error where

import Step.Internal.Prelude

data Error text = Error{ context :: [text] }
    deriving stock (Eq, Show)
