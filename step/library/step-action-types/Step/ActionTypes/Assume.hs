{-# language DataKinds, KindSignatures, FunctionalDependencies, InstanceSigs #-}

module Step.ActionTypes.Assume where

import Step.Internal.Prelude

import Step.ActionTypes.Constructors

class AssumeMovement (act1 :: Action) (act2 :: Action) | act1 -> act2 where
    assumeMovement :: act1 xs x r s m a -> act2 xs x r s m a

class AssumeSurity (act1 :: Action) (act2 :: Action) | act1 -> act2 where
    assumeSurity :: Monad m => act1 xs x r s m a -> act2 xs x r s m a

instance AssumeMovement Any Move where
    assumeMovement = Move

instance AssumeMovement Atom AtomicMove where
    assumeMovement = AtomicMove

instance AssumeSurity Any Sure where
  assumeSurity :: forall xs x r s m a. Monad m => Any xs x r s m a -> Sure xs x r s m a
  assumeSurity = r
    where
      r :: forall a'. Any xs x r s m a' -> Sure xs x r s m a'
      r = \case
        Any_Lift x -> Sure_Lift x
        Any_Ask f -> Sure_Ask f
        Any_Get f -> Sure_Get f
        Any_Next f -> Sure_Next f
        Any_Commit n f -> Sure_Commit n f
        Any_Join x -> Sure_Join (r (fmap r x))
        Any_Fail _ -> error "assumeSurity: assumption failed"

instance AssumeSurity Query SureQuery where
  assumeSurity :: forall xs x r s m a. Monad m => Query xs x r s m a -> SureQuery xs x r s m a
  assumeSurity = r
    where
      r :: forall a'. Query xs x r s m a' -> SureQuery xs x r s m a'
      r = \case
        Query_Lift x -> SureQuery_Lift x
        Query_Ask f -> SureQuery_Ask f
        Query_Get f -> SureQuery_Get f
        Query_Next f -> SureQuery_Next f
        Query_Join x -> SureQuery_Join (r (fmap r x))
        Query_Fail _ -> error "assumeSurity: assumption failed"
