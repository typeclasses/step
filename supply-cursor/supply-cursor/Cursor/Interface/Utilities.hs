module Cursor.Interface.Utilities
  (
    commitAlternative,
  )
  where

import Essentials
import Cursor.Interface.Type

import Integer (Positive)

commitAlternative :: Monad m =>
       (Positive -> m Advancement)
    -> (Positive -> m Advancement)
    -> (Positive -> m Advancement)
commitAlternative a b n = a n >>= \case
    AdvanceSuccess -> pure AdvanceSuccess
    YouCanNotAdvance{ shortfall = n' } -> b n'
