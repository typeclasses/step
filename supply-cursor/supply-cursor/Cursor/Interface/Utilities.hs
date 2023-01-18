module Cursor.Interface.Utilities where

import Essentials
import Cursor.Interface.Type

import Integer (Positive)

commitAlt :: Monad m =>
       (Positive -> m Advancement)
    -> (Positive -> m Advancement)
    -> (Positive -> m Advancement)
commitAlt a b n = a n >>= \case
    AdvanceSuccess -> pure AdvanceSuccess
    YouCanNotAdvance{ shortfall = n' } -> b n'
