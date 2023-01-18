module Block.ListLike.Unsafe where

import Essentials

import Block.ListLike.Type (NonEmptyListLike (..))
import Data.ListLike (ListLike)
import GHC.Exts (Item)

import qualified Data.ListLike as LL
import qualified Integer

assume :: ListLike c (Item c) => c -> NonEmptyListLike c
assume c = NonEmptyListLike c $ Integer.yolo $ LL.length c
