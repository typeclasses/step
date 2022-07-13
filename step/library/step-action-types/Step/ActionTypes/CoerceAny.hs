{-# language DataKinds, KindSignatures, Unsafe #-}

module Step.ActionTypes.CoerceAny where

import Step.Internal.Prelude

import Optics
import qualified Optics

import Step.ActionTypes.Constructors

import qualified Step.ActionTypes.Coerce as Action

class CoerceAny (k :: ActionKind)
  where
    anyIsoUnsafe :: (Monad m1, Monad m2) => Iso
        (k error1 m1 a1)
        (k error2 m2 a2)
        (Any error1 m1 a1)
        (Any error2 m2 a2)

toAny :: forall k error m a. CoerceAny k => Monad m => k error m a -> Any error m a
toAny = view anyIsoUnsafe

fromAnyUnsafe :: forall k error m a. CoerceAny k => Monad m => Any error m a -> k error m a
fromAnyUnsafe = review anyIsoUnsafe

instance CoerceAny Any where anyIsoUnsafe = Optics.coerced
instance CoerceAny Query where anyIsoUnsafe = Optics.coerced
instance CoerceAny Move where anyIsoUnsafe = Optics.coerced
instance CoerceAny Atom where anyIsoUnsafe = Optics.coerced
instance CoerceAny AtomicMove where anyIsoUnsafe = Optics.coerced

instance CoerceAny Sure where
  anyIsoUnsafe = iso f g
    where
      f (Sure a) = Any $ a <&> Right
      g (Any a) = Sure $ a <&> \case
          Left _ -> error "Any to Sure coerce failed"
          Right x -> x

instance CoerceAny SureQuery where
  anyIsoUnsafe = re (Action.coerced @Sure @SureQuery) % anyIsoUnsafe

instance CoerceAny Fail where
  anyIsoUnsafe = iso f g
    where
      f (Fail e) = Any $ return $ Left e
      g (Any a) = Fail $ a >>= \case
          Left e -> e
          Right _ -> error "Any to Fail coerce failed"
