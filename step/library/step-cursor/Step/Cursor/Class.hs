{-# language AllowAmbiguousTypes, FlexibleContexts #-}

module Step.Cursor.Class
  (
    Cursory (..),
  )
  where

import Step.Internal.Prelude

import Step.Cursor.Type (Cursor (..))

import Step.Cursor.ChunkStream (Stream)
import Step.Cursor.AdvanceResult (AdvanceResult)

class Monad (CursoryContext m) => Cursory m
  where
    type CursoryText m :: Type
    type CursoryChar m :: Type
    type CursoryContext m :: Type -> Type

    curse :: Cursor (CursoryText m) (CursoryChar m) m (CursoryContext m)
