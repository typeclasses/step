module Step.Extent.BufferedStream where

import Step.Internal.Prelude

import Step.BufferedStream.Base (BufferedStream)
import qualified Step.BufferedStream.Base as BufferedStream

import Step.Nontrivial.Base (Nontrivial)
import qualified Step.Nontrivial.List as Nontrivial

import qualified ListT

import qualified Monad

newtype Extent m a =
    Extent (ListT (StateT (BufferedStream m a) m) (Nontrivial a))

while :: (Monad m, ListLike text char) => (char -> Bool) -> Extent m text
while ok = Extent $ fix \r ->
    ListT $ BufferedStream.considerChunk f <&> Monad.join <&> \case
        Nothing -> ListT.Nil
        Just x -> ListT.Cons x r
  where
    f x = case Nontrivial.takeWhile ok x of
        Nothing -> (0, Nothing)
        Just y -> (Nontrivial.length x, Just y)
