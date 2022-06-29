module Step.CountingBufferedStream.Tentative where

import Step.Internal.Prelude

import Step.Tentative.Base (Tentative (Tentative))

import Step.CountingBufferedStream.Base (CountingBufferedStream)
import qualified Step.CountingBufferedStream.Base as CountingBufferedStream

takeChar :: Monad m => ListLike chunk char => Tentative (CountingBufferedStream m chunk) m (Maybe char)
takeChar = Tentative (CountingBufferedStream.unconsCharTentative)
