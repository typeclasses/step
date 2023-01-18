module Pushback.StackContainer.Examples
  (
    {- * Examples -} list, sequence,
  )
  where

import Essentials
import Pushback.StackContainer.Type

import Data.Sequence (Seq)

import qualified Data.List as List
import qualified Data.Sequence as Seq

list :: StackContainer [item] item
list = StackContainer (:) List.uncons

sequence :: StackContainer (Seq item) item
sequence = StackContainer (Seq.:<|) $
    Seq.viewl
    >>> \case
        Seq.EmptyL -> Nothing
        x Seq.:< xs -> Just (x, xs)
