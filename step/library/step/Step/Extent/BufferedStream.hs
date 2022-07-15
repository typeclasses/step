module Step.Extent.BufferedStream where

import Step.Internal.Prelude

import Step.BufferedStream.Base (BufferedStream)
import qualified Step.BufferedStream.State as BufferedStream.State

import Step.Nontrivial.Base (Nontrivial)

import qualified ListT

newtype Extent m a =
    Extent (ListT (StateT (BufferedStream m a) m) (Nontrivial a))

while :: (Monad m, ListLike text char) => (char -> Bool) -> Extent m text
while ok = Extent $ fix \r ->
    ListT $ BufferedStream.State.takeChunkWhile ok <&> \case
        Nothing -> ListT.Nil
        Just x -> ListT.Cons x r
