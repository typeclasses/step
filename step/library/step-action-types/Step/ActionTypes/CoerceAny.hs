{-# language DataKinds, KindSignatures, Unsafe #-}

module Step.ActionTypes.CoerceAny where

import Step.Internal.Prelude

import Optics

import Step.ActionTypes.Constructors

import qualified Step.ActionTypes.Coerce as Action

import qualified Either

class CoerceAny (act :: Action) where
    anyIsoUnsafe :: (Monad m1, Monad m2) =>
        Iso (act xs1 x1 r1 s1 m1 a1) (act xs2 x2 r2 s2 m2 a2) (Any xs1 x1 r1 s1 m1 a1) (Any xs2 x2 r2 s2 m2 a2)

toAny :: forall act xs x r s m a. CoerceAny act => Monad m => act xs x r s m a -> Any xs x r s m a
toAny = view anyIsoUnsafe

fromAnyUnsafe :: forall act xs x r s m a. CoerceAny act => Monad m => Any xs x r s m a -> act xs x r s m a
fromAnyUnsafe = review anyIsoUnsafe

instance CoerceAny Any where anyIsoUnsafe = Optics.coerced
instance CoerceAny Query where anyIsoUnsafe = Optics.coerced
instance CoerceAny Move where anyIsoUnsafe = Optics.coerced
instance CoerceAny Atom where anyIsoUnsafe = Optics.coerced
instance CoerceAny AtomicMove where anyIsoUnsafe = Optics.coerced

instance CoerceAny Sure where
  anyIsoUnsafe = iso f g
    where
      f (Sure a) = Any \c -> a c <&> Right
      g (Any a) = Sure \c -> a c <&> Either.fromRight (error "Any to Sure coerce failed")

instance CoerceAny SureQuery where
  anyIsoUnsafe = re (Action.coerced @Sure @SureQuery) % anyIsoUnsafe

instance CoerceAny Fail where
  anyIsoUnsafe = iso f g
    where
      f (Fail a) = Any \c -> a c *> (ask <&> Left)
      g (Any a) = Fail \c -> a c *> ask
