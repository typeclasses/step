module Block.ListLike.Unsafe
  (
    assume,
  )
  where

import {-# source #-} Block.ListLike.Type (NonEmptyListLike (..))

import Data.ListLike (ListLike)
import GHC.Exts (Item)

assume :: ListLike c (Item c) => c -> NonEmptyListLike c
