module Step.Extent.ListT where

import Step.Internal.Prelude

import Step.Nontrivial.Base (Nontrivial)

import qualified Step.Nontrivial.List as Nontrivial

import qualified ListT

newtype Extent m a =
    Extent (ListT (StateT (ListT m a) m) a)

while :: (Monad m, ListLike text char) => (char -> Bool) -> Extent m (Nontrivial text)
while ok = Extent $ fix \r ->
    ListT $ StateT \xs -> ListT.next xs <&> \case
        ListT.Nil -> (ListT.Nil, ListT.empty)
        ListT.Cons x xs' -> case Nontrivial.span ok x of
            Nontrivial.None -> (ListT.Nil, pure x <|> xs')
            Nontrivial.All -> (ListT.Cons x r, xs')
            Nontrivial.Split a b -> (ListT.Cons a ListT.empty, pure b <|> xs')
