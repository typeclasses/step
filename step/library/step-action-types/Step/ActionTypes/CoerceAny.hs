{-# language DataKinds, KindSignatures, Unsafe #-}

module Step.ActionTypes.CoerceAny where

import Step.Internal.Prelude

import Optics

import Step.ActionTypes.Constructors

import qualified Step.ActionTypes.Coerce as Action

class CoerceAny (act :: Action) where
    anyIsoUnsafe :: (Monad m1, Monad m2) =>
        Iso (act m1 e1 a1) (act m2 e2 a2) (Any m1 e1 a1) (Any m2 e2 a2)

toAny :: forall act m e a. CoerceAny act => Monad m => act m e a -> Any m e a
toAny = view anyIsoUnsafe

fromAnyUnsafe :: forall act m e a. CoerceAny act => Monad m => Any m e a -> act m e a
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
