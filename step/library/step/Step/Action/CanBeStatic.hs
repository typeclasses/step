{-# language DataKinds, KindSignatures, Trustworthy #-}

module Step.Action.CanBeStatic where

import Step.Internal.Prelude

import Step.Action.Constructors

class CanBeStatic (k :: ActionKind)
  where
    trivial :: Monad m => a -> k config cursor error m a

instance CanBeStatic Any where
    trivial x = Any \_ -> StateT \s -> return (Right x, s)

instance CanBeStatic Query where
    trivial x = Query \_ -> StateT \s -> return (Right x, s)

instance CanBeStatic Atom where
    trivial x = Atom \_ -> StateT \s -> return (Right x, s)

instance CanBeStatic Sure where
    trivial x = Sure \_ -> StateT \s -> return (x, s)

instance CanBeStatic SureQuery where
    trivial x = SureQuery \_ -> StateT \s -> return (x, s)
