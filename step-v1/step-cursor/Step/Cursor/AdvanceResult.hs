module Step.Cursor.AdvanceResult
  (
    AdvanceResult (..),
  )
  where

import Step.Internal.Prelude

data AdvanceResult =
    AdvanceSuccess
  | YouCanNotAdvance{ shortfall :: Positive Natural }
