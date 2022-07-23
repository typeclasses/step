{-# language DataKinds, KindSignatures #-}

module Step.ActionTypes.ChangeBase where

import Step.Internal.Prelude

import Step.ActionTypes.Constructors

class ChangeBase (act :: Action) where
    changeBase :: Functor m2 => (forall x. m1 x -> m2 x) -> act m1 e a -> act m2 e a

instance ChangeBase Any where
    changeBase f (Any a) = Any $ f a <&> \case Left e -> Left (f e); Right x -> Right x

instance ChangeBase Query where
    changeBase f (Query a) = Query $ f a <&> \case Left e -> Left (f e); Right x -> Right x

instance ChangeBase Move where
    changeBase f (Move a) = Move $ f a <&> \case Left e -> Left (f e); Right x -> Right x

instance ChangeBase Atom where
    changeBase f (Atom a) = Atom $ f a <&> \case Left e -> Left (f e); Right x -> Right x

instance ChangeBase AtomicMove where
    changeBase f (AtomicMove a) = AtomicMove $ f a <&> \case Left e -> Left (f e); Right x -> Right x

instance ChangeBase Sure where
    changeBase f (Sure a) = Sure $ f a

instance ChangeBase SureQuery where
    changeBase f (SureQuery a) = SureQuery $ f a

instance ChangeBase Fail where
    changeBase f (Fail a) = Fail $ f a
