{-# language DataKinds, KindSignatures, Trustworthy #-}

module Step.ActionTypes.Returnable where

import Step.Internal.Prelude

import Step.ActionTypes.Constructors

class Returnable (k :: ActionKind)
  where
    trivial :: Monad m => a -> k cursor error m a

instance Returnable Any where
    trivial x = Any $ StateT \s -> return (Right x, s)

instance Returnable Query where
    trivial x = Query $ StateT \s -> return (Right x, s)

instance Returnable Atom where
    trivial x = Atom $ StateT \s -> return (Right x, s)

instance Returnable Sure where
    trivial x = Sure $ StateT \s -> return (x, s)

instance Returnable SureQuery where
    trivial x = SureQuery $ StateT \s -> return (x, s)
