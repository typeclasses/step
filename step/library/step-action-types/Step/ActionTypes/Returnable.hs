{-# language DataKinds, KindSignatures #-}

module Step.ActionTypes.Returnable where

import Step.Internal.Prelude

import Step.ActionTypes.Constructors

class Returnable (act :: Action) where
    trivial :: Monad m => a -> act xs x r s e m a

instance Returnable Any where trivial = Any_Lift . return
instance Returnable Query where trivial = Query_Lift . return
instance Returnable Atom where trivial = Atom . return . return
instance Returnable Sure where trivial = Sure . return
instance Returnable SureQuery where trivial = SureQuery . return
