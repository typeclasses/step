{-# language DataKinds, KindSignatures, Trustworthy #-}

module Step.ActionTypes.Returnable where

import Step.Internal.Prelude

import Step.ActionTypes.Constructors

class Returnable (k :: ActionKind) where
    trivial :: Monad m => a -> k error m a

instance Returnable Any where trivial = Any . return . Right
instance Returnable Query where trivial = Query . return . Right
instance Returnable Atom where trivial = Atom . return . Right
instance Returnable Sure where trivial = Sure . return
instance Returnable SureQuery where trivial = SureQuery . return
