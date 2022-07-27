{-# language FlexibleInstances, FunctionalDependencies #-}

module Step.Buffer.HasBuffer (HasBuffer (..)) where

import Step.Internal.Prelude

import Step.Buffer.Buffer (Buffer)

import qualified Optics

class HasBuffer xs x a | a -> xs x where
    buffer :: Lens' a (Buffer xs x)

instance HasBuffer xs x (Buffer xs x) where
    buffer = Optics.castOptic Optics.simple
