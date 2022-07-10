{-# language DataKinds, KindSignatures, Trustworthy #-}

module Step.ActionTypes.Returnable where

import Step.Internal.Prelude

import Step.ActionTypes.Constructors

class Returnable (k :: ActionKind)
  where
    trivial :: Monad m => a -> k config cursor error m a

instance Returnable Any where
    trivial x = Any \_ -> StateT \s -> return (Right x, s)

instance Returnable Query where
    trivial x = Query \_ -> StateT \s -> return (Right x, s)

instance Returnable Atom where
    trivial x = Atom \_ -> StateT \s -> return (Right x, s)

instance Returnable Sure where
    trivial x = Sure \_ -> StateT \s -> return (x, s)

instance Returnable SureQuery where
    trivial x = SureQuery \_ -> StateT \s -> return (x, s)
