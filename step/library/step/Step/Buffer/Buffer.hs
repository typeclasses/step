{-# language FlexibleInstances, FlexibleContexts, Trustworthy #-}

module Step.Buffer.Buffer
  (
    Buffer (..),
    isEmpty, empty, (|>),
  )
  where

import Step.Internal.Prelude

import qualified Seq

import Step.Nontrivial (Nontrivial)
import qualified Step.Nontrivial as Nontrivial
import qualified Step.Nontrivial.Drop as Drop

import Step.Cursor (Stream, AdvanceResult (..))
import qualified Step.Cursor as Cursor

import Step.Buffer.BufferResult (BufferResult(..))

data Buffer xs x = Buffer { chunks :: Seq (Nontrivial xs x) }

(|>) :: Buffer xs x -> Nontrivial xs x -> Buffer xs x
Buffer xs |> x = Buffer (xs Seq.|> x)

isEmpty :: Buffer xs x -> Bool
isEmpty = Seq.null . chunks

empty :: Buffer xs x
empty = Buffer{ chunks = Seq.empty }
