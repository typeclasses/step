module Cursor.Advancement.Utilities
  (
    {- * Utilities -} commitAlternative, shortfallNatural, minusShortfall,
  )
  where

import Essentials

import Cursor.Advancement.Type (Advancement (..))
import Integer (Positive, Natural)
import Prelude ((-))
import Block (Shortfall (..))

import qualified Integer.Positive as Positive

commitAlternative :: Monad m =>
       (Positive -> m Advancement)
    -> (Positive -> m Advancement)
    -> (Positive -> m Advancement)
commitAlternative a b n = a n >>= \case
    AdvanceSuccess -> pure AdvanceSuccess
    YouCanNotAdvance (Shortfall n') -> b n'

shortfallNatural :: Advancement -> Natural
shortfallNatural = \case
    AdvanceSuccess -> 0
    YouCanNotAdvance (Shortfall n') -> Positive.toNatural n'

minusShortfall :: Advancement -> Natural -> Natural
minusShortfall = \case
    AdvanceSuccess -> id
    YouCanNotAdvance (Shortfall n') -> \x -> x - Positive.toNatural n'
