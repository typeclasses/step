{-# language DataKinds, FlexibleContexts, FlexibleInstances, KindSignatures, StandaloneKindSignatures, TypeFamilies #-}
{-# language DerivingVia, GeneralizedNewtypeDeriving, StandaloneDeriving #-}

module Step.Document.Base
  (
    -- * Types
    DocumentParsing, Config (..), Error (..),
    -- * Running parsers
    parse, parseOnly,
  )
  where

import Step.Internal.Prelude

import Step.Document.Memory (DocumentMemory)
import qualified Step.Document.Memory as DocumentMemory

import Step.Document.Lines (Char)

import qualified Step.ActionTypes as Action
import Step.ActionTypes.Unsafe (Any (Any))

import qualified Step.Classes.Base as Class

import Text (Text)

import Step.LookingAhead (Prophetic (forecast))
import qualified Step.LookingAhead (Prophetic (..))

import Step.Advancement (Progressive (..))

---

data Config = Config{ configContext :: [Text] }
    deriving stock (Eq, Show)

instance Default Config
  where
    def = Config{ configContext = [] }

instance Class.HasContextStack Config where
    contextStackLens = lens configContext \x y -> x{ configContext = y }

---

data Error = Error{ errorContext :: [Text] }
    deriving stock (Eq, Show)

---

newtype DocumentParsing text char m a =
    DocumentParsing (ReaderT Config (StateT (DocumentMemory text char m) m) a)
    deriving (Functor, Applicative, Monad)
        via (ReaderT Config (StateT (DocumentMemory text char m) m))

instance (Monad m, ListLike text char) => Progressive (DocumentParsing text char m) where
    advance n = DocumentParsing (advance n)

instance (Monad m, ListLike text char) => Prophetic (DocumentParsing text char m) where
    type Text (DocumentParsing text char m) = text
    type Char (DocumentParsing text char m) = char
    forecast = changeBaseListT DocumentParsing forecast

instance (Monad m, ListLike text char) => Class.Char1 (DocumentParsing text char m) where
    type Text (DocumentParsing text char m) = text
    type Char (DocumentParsing text char m) = char
    peekCharMaybe = DocumentParsing Class.peekCharMaybe
    considerChar f = DocumentParsing (Class.considerChar f)

instance (ListLike text char, Monad m) => Class.Locating (DocumentParsing text char m) where
    position = DocumentParsing Class.position

instance (ListLike text char, Monad m) => Class.Fallible (DocumentParsing text char m) where
    type Error (DocumentParsing text char m) = Error
    failure = DocumentParsing $ ReaderT \c -> return Error{ errorContext = configContext c }

instance (ListLike text char, Monad m) => Class.Configure (DocumentParsing text char m) where
    type Config (DocumentParsing text char m) = Config
    configure f (DocumentParsing a) = DocumentParsing (Class.configure f a)

---

parse :: Monad m => Action.Is kind Any =>
    Config -> kind (DocumentParsing text char m) Error value
    -> StateT (DocumentMemory text char m) m (Either Error value)
parse config p =
    p & Action.cast @Any & \(Any (DocumentParsing p')) -> runReaderT p' config >>= \case
        Left (DocumentParsing errorMaker) -> Left <$> runReaderT errorMaker config
        Right x -> return (Right x)

parseOnly :: Action.Is kind Any => Monad m => Char char => ListLike text char =>
    Config -> kind (DocumentParsing text char m) Error value -> ListT m text -> m (Either Error value)
parseOnly config p xs =
    evalStateT (parse config p) (DocumentMemory.fromListT xs)
