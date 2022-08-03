{-# language DataKinds, KindSignatures, Trustworthy #-}

module Step.ActionTypes.Returnable where

import Step.Internal.Prelude

import Step.ActionTypes.Constructors

class Returnable (act :: Action) where
    trivial :: Monad m => a -> act xs x r s m a

instance Returnable Any where trivial x = Any \_ -> return (Right x)
instance Returnable Query where trivial x = Query \_ -> return (Right x)
instance Returnable Atom where trivial x = Atom \_ -> return (Right x)
instance Returnable Sure where trivial x = Sure \_ -> return x
instance Returnable SureQuery where trivial x = SureQuery \_ -> return x
