module Step.Extent.Base where

import Step.Internal.Prelude

import Step.BufferedStream.Base (BufferedStream)

import Step.Nontrivial.Base (Nontrivial)
import qualified Step.Nontrivial.Base as Nontrivial

import qualified Step.Nontrivial.List as Nontrivial

import qualified ListT

import qualified ListLike

newtype ListExtent m a =
    ListExtent (ListT (StateT (ListT m a) m) a)

while :: (Monad m, ListLike text char) => (char -> Bool) -> ListExtent m (Nontrivial text)
while ok = ListExtent $ fix \r ->
    ListT $ StateT \xs -> ListT.next xs <&> \case
        ListT.Nil -> (ListT.Nil, ListT.empty)
        ListT.Cons x xs' -> case Nontrivial.span ok x of
            Nontrivial.None _ -> (ListT.Nil, pure x <|> xs')
            Nontrivial.All x' -> (ListT.Cons x' r, xs')
            Nontrivial.Split x' remainder -> (ListT.Cons x' ListT.empty, pure remainder <|> xs')
