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

    {-# minimal curse | cursoryRun, cursoryCommit, cursoryInput #-}

    curse :: Cursor (CursoryText m) (CursoryChar m) m (CursoryContext m)
    curse = Cursor{ run = cursoryRun @m, commit = cursoryCommit @m, input = cursoryInput @m }

    cursoryRun :: CursoryContext m a -> m a
    cursoryRun = run curse

    cursoryCommit :: Positive Natural -> CursoryContext m AdvanceResult
    cursoryCommit = commit (curse @m)

    cursoryInput :: Stream (CursoryContext m) (CursoryText m) (CursoryChar m)
    cursoryInput = input (curse @m)
