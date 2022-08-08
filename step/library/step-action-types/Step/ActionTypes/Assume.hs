{-# language DataKinds, KindSignatures, FunctionalDependencies, InstanceSigs #-}

module Step.ActionTypes.Assume where

import Step.Internal.Prelude

import Step.ActionTypes.Constructors

import Step.ActionTypes.Fallible

class AssumeMovement (act1 :: Action) (act2 :: Action) | act1 -> act2 where
    assumeMovement :: act1 xs x r s e m a -> act2 xs x r s e m a

class AssumeSurity (act1 :: Action) (act2 :: Action) | act1 -> act2 where
    assumeSurity :: Monad m => act1 xs x r s e m a -> act2 xs x r s e m a

instance AssumeMovement Any Move where
    assumeMovement = Move

instance AssumeMovement Atom AtomicMove where
    assumeMovement = AtomicMove

instance AssumeSurity Any Sure where
  assumeSurity = Sure . mapError (\_ -> error "assumeSurity: assumption failed")

instance AssumeSurity Query SureQuery where
  assumeSurity = SureQuery . mapError (\_ -> error "assumeSurity: assumption failed")
