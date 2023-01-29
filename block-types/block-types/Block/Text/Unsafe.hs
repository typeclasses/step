module Block.Text.Unsafe
  (
    assume,
  )
  where

import Block.Text.Type
import Essentials

import Data.Text (Text)

import qualified Block.ListLike.Unsafe as LL

assume :: Text -> Text1
assume = Text1 . LL.assume
