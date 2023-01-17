module Pushback.Container where

import Essentials

import Data.Sequence (Seq)

import qualified Data.List as List
import qualified Data.Sequence as Seq

data StackContainer container item = StackContainer
    { push :: item -> container -> container
    , pop :: container -> Maybe (item, container)
    }

list :: StackContainer [item] item
list = StackContainer (:) List.uncons

sequence :: StackContainer (Seq item) item
sequence = StackContainer (Seq.:<|) $
    Seq.viewl >>> \case
        Seq.EmptyL -> Nothing
        x Seq.:< xs -> Just (x, xs)
