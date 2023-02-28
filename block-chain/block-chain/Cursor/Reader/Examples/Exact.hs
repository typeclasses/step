module Cursor.Reader.Examples.Exact where

import Essentials
import Cursor.Reader.Type
import Cursor.Interface.Orders

import Cursor.Interface (Mode (..), Step (..))
import Block (End (..), biPrefix, itemEquality, BiPrefix (..), WhichOfTwo (..))
import Prelude (Bounded (..), show)
import Data.String (IsString, fromString)
import Cursor.Reader.Utilities (firstJust)

import qualified Block

exact :: forall up action item block.
    block -> ReaderPlus up action 'Write item block Bool
exact = \w -> Reader (go w)
  where
    go w = next >>= \case
        End -> pure True
        Item b -> case biPrefix itemEquality Front (b, w) of
            NoPrefixRelation -> pure False
            BothPrefix -> commitPositive (Block.length b) $> True
            IsPrefix First _ rem -> commitPositive (Block.length b) *> go rem
            IsPrefix Second _ _ -> commitPositive (Block.length w) $> True

enum :: (Enum a, Bounded a, Show a, IsString block) =>
    ReaderPlus up action 'Write item block (Maybe a)
enum = firstJust $ [minBound .. maxBound] <&> \m ->
    exact (fromString $ show m) <&> \x -> if x then Just m else Nothing
