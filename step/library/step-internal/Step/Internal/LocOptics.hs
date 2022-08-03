{-# language Trustworthy #-}

module Step.Internal.LocOptics
  (
    lineLens,
    columnLens,
  )
  where

import Loc (Line, Column, Loc)
import qualified Loc

import Optics

lineLens :: Lens' Loc Line
lineLens = lens Loc.locLine (\x y -> Loc.loc y (Loc.locColumn x))

columnLens :: Lens' Loc Column
columnLens = lens Loc.locColumn (\x y -> Loc.loc (Loc.locLine x) y)
