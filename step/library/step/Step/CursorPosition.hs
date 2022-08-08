{-# language GeneralizedNewtypeDeriving #-}

module Step.CursorPosition
  (
    CursorPosition (..),
    increase, strictlyIncrease,
  )
  where

import Step.Internal.Prelude

import qualified Positive

newtype CursorPosition = CursorPosition Natural
    deriving newtype (Eq, Ord, Show, Num)

strictlyIncrease :: Positive Natural -> Endo CursorPosition
strictlyIncrease x =
    Endo \(CursorPosition p) ->
        CursorPosition (p + review Positive.refine x)

increase :: Natural -> Endo CursorPosition
increase x = case preview Positive.refine x of
    Nothing -> mempty
    Just x' -> strictlyIncrease x'
