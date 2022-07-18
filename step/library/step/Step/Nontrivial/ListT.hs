{-# language Safe #-}

module Step.Nontrivial.ListT where

import Step.Internal.Prelude

import Step.Nontrivial.Base (Nontrivial)
import qualified Step.Nontrivial.Base as Nontrivial

import Applicative (empty)

filter :: ListLike a c => Monad m => ListT m a -> ListT m (Nontrivial a c)
filter xs = do
    x <- xs
    case Nontrivial.refine x of
        Nothing -> empty
        Just y -> return y
