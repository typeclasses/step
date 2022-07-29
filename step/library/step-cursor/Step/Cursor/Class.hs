{-# language AllowAmbiguousTypes, FlexibleContexts #-}

module Step.Cursor.Class
  (
    Cursory (..),
  )
  where

import Step.Internal.Prelude

import Step.Cursor.Type (Cursor (..))

import Step.Cursor.Stream (Stream)
import Step.Cursor.AdvanceResult (AdvanceResult)

class Cursory m
  where
    type CursoryText m :: Type
    type CursoryChar m :: Type
    type CursoryParam m :: Type
    type CursoryInternalState m :: Type
    type CursoryState m :: Type

    curse :: Cursor (CursoryText m) (CursoryChar m) (CursoryParam m) (CursoryInternalState m) (CursoryState m) m
