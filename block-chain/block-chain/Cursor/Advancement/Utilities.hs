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
       (Positive -> m (Advancement () ()))
    -> (Positive -> m (Advancement () ()))
    -> (Positive -> m (Advancement () ()))
commitAlternative a b n = a n >>= \case
    AdvanceSuccess _ -> pure $ AdvanceSuccess ()
    YouCanNotAdvance (Shortfall n') _ -> b n'

shortfallNatural :: Advancement a b -> Natural
shortfallNatural = \case
    AdvanceSuccess _ -> 0
    YouCanNotAdvance (Shortfall s) _ -> Positive.toNatural s

minusShortfall :: Advancement a b -> Natural -> Natural
minusShortfall = \case
    AdvanceSuccess _ -> id
    YouCanNotAdvance (Shortfall s) _ -> \x -> x - Positive.toNatural s
