module Step.Cursor.Tentative where

import Step.Internal.Prelude

import Step.Tentative.Base (Tentative (Tentative))

import Step.Cursor.Base (Cursor)
import qualified Step.Cursor.Base as Cursor

takeChar :: Monad m => ListLike text char => Tentative (Cursor m text) m (Maybe char)
takeChar = Tentative (Cursor.unconsCharTentative)
