module Cursor.Advancement.Utilities
  (
    {- * Utilities -} commitAlternative, shortfallNatural, minusShortfall,
  )
  where

import Essentials

import Cursor.Advancement.Type (Advancement (..))
import Integer (Positive, Natural)
import Prelude ((-))

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

minusShortfall :: Advancement -> Natural -> Natural
minusShortfall = \case
    AdvanceSuccess -> id
    YouCanNotAdvance{ shortfall } -> \x -> x - Positive.toNatural shortfall
