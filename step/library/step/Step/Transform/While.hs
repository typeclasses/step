{-# language FlexibleInstances, RankNTypes, TypeFamilies #-}
{-# language DerivingStrategies, GeneralizedNewtypeDeriving #-}

module Step.Transform.While where

import Step.Internal.Prelude

import Step.Classes.Base

import GHC.Exts (Item)

import Step.TakeOrLeave

import qualified ListLike

newtype While text m a =
    While (ReaderT (Item text -> Bool) m a)
    deriving newtype (Functor, Applicative, Monad, MonadTrans)

instance (Peek1 m, (Text m ~ text)) => Peek1 (While text m) where

    type Text (While text m) = Text m

    peekCharMaybe = While $ ReaderT \ok -> peekCharMaybe <&> \case
        Just x | ok x -> Just x
        _ -> Nothing

instance (Take1 m, (Text m ~ text)) => Take1 (While text m) where

    considerChar f = While $ ReaderT \ok ->
        (considerChar \x -> if ok x then over leavePrism Just (f x) else Leave Nothing)
        <&> \case
            Just (Take x) -> Just (Take x)
            Just (Leave Nothing) -> Nothing
            Just (Leave (Just x)) -> Just (Leave x)
            Nothing -> Nothing

instance Locating m => Locating (While text m) where
    position = lift position

instance Fallible m => Fallible (While text m) where
    type Error (While text m) = Error m
    failure = lift failure

instance (SkipTextNonAtomic m, (Text m ~ text)) => SkipTextNonAtomic (While text m) where
    skipTextNonAtomic x = While $ ReaderT \ok ->
        if ListLike.all ok x then skipTextNonAtomic x else return False

instance FillBuffer1 m => FillBuffer1 (While text m) where
    fillBuffer1 = lift fillBuffer1
