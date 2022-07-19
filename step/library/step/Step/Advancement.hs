module Step.Advancement where

import Step.Internal.Prelude

data AdvanceResult = Advanced | InsufficientInput{ charactersLacking :: Positive Natural }
