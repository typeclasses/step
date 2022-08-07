{-# language DataKinds, KindSignatures, FunctionalDependencies #-}

module Step.ActionTypes.Assume where

import Step.Internal.Prelude

import Step.ActionTypes.Constructors

class AssumeMovement (act1 :: Action) (act2 :: Action) | act1 -> act2 where
    assumeMovement :: act1 xs x r s m a -> act2 xs x r s m a

class AssumeSurity (act1 :: Action) (act2 :: Action) | act1 -> act2 where
    assumeSurity :: act1 xs x r s m a -> act2 xs x r s m a

instance AssumeMovement Any Move where

instance AssumeMovement Atom AtomicMove where

instance AssumeSurity Any Sure where

instance AssumeSurity Query SureQuery where
