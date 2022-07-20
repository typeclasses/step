{-# language FlexibleContexts, FlexibleInstances, RankNTypes, TypeFamilies #-}
{-# language DerivingStrategies, GeneralizedNewtypeDeriving #-}

module Step.Transform.While where

import Step.Internal.Prelude

import qualified Step.Classes.Base as C

import GHC.Exts (Item)

import Step.TakeOrLeave

import qualified ListLike

newtype While text char m a =
    While (ReaderT (char -> Bool) m a)
    deriving newtype (Functor, Applicative, Monad, MonadTrans)

instance (C.Char1 m, ListLike text char, C.Char m ~ char) =>
    C.Char1 (While text char m)
  where

    type Text (While text char m) = text
    type Char (While text char m) = char

    peekCharMaybe = While $ ReaderT \ok -> C.peekCharMaybe <&> \case
        Just x | ok x -> Just x
        _ -> Nothing

    considerChar (C.Consideration1 f) = While $ ReaderT \ok ->
        (C.considerChar $ C.Consideration1 \x -> if ok x then over leavePrism Just (f x) else Leave Nothing)
        <&> \case
            Just (Take x) -> Just (Take x)
            Just (Leave Nothing) -> Nothing
            Just (Leave (Just x)) -> Just (Leave x)
            Nothing -> Nothing

instance (C.Locating m, ListLike text char, C.Char m ~ char) => C.Locating (While text char m) where
    position = lift C.position

instance (C.Fallible m, ListLike text char, C.Char m ~ char) => C.Fallible (While text char m) where
    type Error (While text char m) = C.Error m
    failure = lift C.failure

instance (ListLike text char, C.SkipTextNonAtomic m, C.Text m ~ text, C.Char m ~ char, Eq char) => C.SkipTextNonAtomic (While text char m) where
    skipTextNonAtomic x = While $ ReaderT \ok ->
        if ListLike.all ok x then C.skipTextNonAtomic x else return False

instance (C.FillBuffer1 m, ListLike text char, C.Char m ~ char) => C.FillBuffer1 (While text char m) where
    fillBuffer1 = lift C.fillBuffer1
