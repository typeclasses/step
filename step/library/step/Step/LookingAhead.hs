{-# language FunctionalDependencies #-}

module Step.LookingAhead where

import Step.Internal.Prelude

import Step.Nontrivial.Base

class Prophetic m text char where
    forecast :: ListT m (Nontrivial text char)
