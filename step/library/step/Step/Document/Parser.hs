{-# language DataKinds, FlexibleContexts, FlexibleInstances, KindSignatures, StandaloneKindSignatures, TypeFamilies #-}
{-# language DerivingVia, GeneralizedNewtypeDeriving, StandaloneDeriving #-}

module Step.Document.Parser where

import Step.Internal.Prelude

import Step.DocumentMemory.Base (DocumentMemory)
import qualified Step.DocumentMemory.Base as DocumentMemory

import Step.Document.Config (Config)
import qualified Step.Document.Config as Config

import Step.Document.Error (Error (Error))
import qualified Step.Document.Error as Error

import Step.LineHistory.Char (Char)

import Step.ActionTypes (Action, FunctorialAction, MonadicAction)
import qualified Step.ActionTypes as Action
import Step.ActionTypes.Unsafe (Any (Any))

import qualified Monad

import qualified Step.Classes as Class

parse :: Monad m => Action.Is kind Any =>
    Config -> kind (DocumentParsing text m) Error value
    -> StateT (DocumentMemory text m) m (Either Error value)
parse config p =
    p & Action.cast @Any & \(Any (DocumentParsing p')) -> runReaderT p' config >>= \case
        Left (DocumentParsing errorMaker) -> Left <$> runReaderT errorMaker config
        Right x -> return (Right x)

parseOnly :: Action.Is kind Any => Monad m => Char char => ListLike text char =>
    Config -> kind (DocumentParsing text m) Error value -> ListT m text -> m (Either Error value)
parseOnly config p xs =
    evalStateT (parse config p) (DocumentMemory.fromListT xs)

newtype DocumentParsing text m a =
    DocumentParsing (ReaderT Config (StateT (DocumentMemory text m) m) a)
    deriving (Functor, Applicative, Monad)
        via (ReaderT Config (StateT (DocumentMemory text m) m))

instance Monad m => Class.Peek1 (DocumentParsing text m) where
    type Text (DocumentParsing text m) = text
    peekCharMaybe = DocumentParsing Class.peekCharMaybe

instance Monad m => Class.Take1 (DocumentParsing text m) where
    considerChar f = DocumentParsing (Class.considerChar f)

instance Monad m => Class.TakeAll (DocumentParsing text m) where
    takeAll = DocumentParsing Class.takeAll

instance Monad m => Class.Locating (DocumentParsing text m) where
    position = DocumentParsing Class.position

instance Monad m => Class.Fallible (DocumentParsing text m) where
    type Error (DocumentParsing text m) = Error
    failure = DocumentParsing $ ReaderT \c -> return Error{ Error.context = Config.context c }
