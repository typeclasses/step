module Cursor.Interface.Utilities
  (
    commitAlternative,
    shortfallNatural,
  )
  where

import Essentials
import Cursor.Interface.Type

import Integer (Positive, Natural)

import qualified Integer.Positive as Positive

commitAlternative :: Monad m =>
       (Positive -> m Advancement)
    -> (Positive -> m Advancement)
    -> (Positive -> m Advancement)
commitAlternative a b n = a n >>= \case
    AdvanceSuccess -> pure AdvanceSuccess
    YouCanNotAdvance{ shortfall = n' } -> b n'

shortfallNatural :: Advancement -> Natural
shortfallNatural = \case
    AdvanceSuccess -> 0
    YouCanNotAdvance{ shortfall } -> Positive.toNatural shortfall
