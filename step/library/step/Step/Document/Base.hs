{-# language DataKinds, FlexibleContexts, FlexibleInstances, KindSignatures, StandaloneKindSignatures #-}
{-# language DerivingVia, GeneralizedNewtypeDeriving, StandaloneDeriving #-}

module Step.Document.Base
  (
    curse,
    -- * Types
    DocumentParsing, Config (..), Error (..),
    -- * Running parsers
    parse, parseOnly,
  )
  where

import Step.Internal.Prelude

import Step.Document.Memory (DocumentMemory)
import qualified Step.Document.Memory as DocumentMemory

import qualified Step.Document.Lines as Lines

import qualified Step.ActionTypes as Action
import Step.ActionTypes.Unsafe (Any (Any))

import Text (Text)

import Step.Input.Cursor (Cursor, curse)
import qualified Step.Input.Cursor as Cursor

import Step.Document.Locating (Locating (..))

import Step.Configuration (HasContextStack, contextStackLens, Configure, configure)
import qualified Step.Configuration as Config

import Step.Failure (Fallible)
import qualified Step.Failure as F

import Step.Input.Counter (Counting)
import qualified Step.Input.Counter as Counting

import Step.Input.Counter (cursorPosition)

import Step.Input.Stream (Stream)

import Step.Nontrivial (Nontrivial)

---

data Config = Config{ configContext :: [Text] }
    deriving stock (Eq, Show)

instance Default Config
  where
    def = Config{ configContext = [] }

instance HasContextStack Config where
    contextStackLens = lens configContext \x y -> x{ configContext = y }

---

data Context m text char = Context{ config :: Config, input :: Stream m (Nontrivial text char) }

contextConfigLens = lens config \x y -> x{ config = y }

contextInputLens = lens input \x y -> x{ input = y }

---

data Error = Error{ errorContext :: [Text] }
    deriving stock (Eq, Show)

---

newtype DocumentParsing text char m a =
    DocumentParsing (ReaderT (Context m text char) (StateT (DocumentMemory text char) m) a)
    deriving (Functor, Applicative, Monad)
        via (ReaderT (Context m text char) (StateT (DocumentMemory text char) m))

instance (Monad m, ListLike text char, Lines.Char char) => Cursor (DocumentParsing text char m) where
    type Text (DocumentParsing text char m) = text
    type Char (DocumentParsing text char m) = char
    curse = DocumentParsing $ ReaderT \Context{ input } ->
        return $ Cursor.rebaseSession (DocumentParsing . lift) (DocumentMemory.curse input)

        -- DocumentMemory.curse & Cursor.rebaseSession \(ReaderT f) ->
        --     DocumentParsing $ ReaderT \Context{ config, input } ->
        -- _
        -- (DocumentParsing . lift) DocumentMemory.curse

instance (ListLike text char, Monad m, Lines.Char char) => Locating (DocumentParsing text char m) where
    position = DocumentParsing $ ReaderT \Context{ input } ->
        runReaderT (position @(ReaderT (Stream m (Nontrivial text char)) (StateT (DocumentMemory text char) m))) input

instance (ListLike text char, Monad m) => Fallible (DocumentParsing text char m) where
    type Error (DocumentParsing text char m) = Error
    failure = DocumentParsing $ ReaderT \c -> return Error{ errorContext = configContext (config c) }

instance (ListLike text char, Monad m) => Configure (DocumentParsing text char m) where
    type Config (DocumentParsing text char m) = Config
    configure f (DocumentParsing a) = DocumentParsing (configure (over contextConfigLens f) a)

instance (ListLike text char, Monad m) => Counting (DocumentParsing text char m) where
    cursorPosition = DocumentParsing Counting.cursorPosition

---

parse :: Monad m => Action.Is kind Any =>
    Config -> Stream m (Nontrivial text char) -> kind (DocumentParsing text char m) Error value
    -> StateT (DocumentMemory text char) m (Either Error value)
parse config input p =
    p & Action.cast @Any & \(Any (DocumentParsing p')) ->
        let ctx = Context{ config, input } in
        runReaderT p' ctx >>= \case
            Left (DocumentParsing errorMaker) -> Left <$> runReaderT errorMaker ctx
            Right x -> return (Right x)

parseOnly :: forall m text char kind value. Action.Is kind Any => Monad m => Lines.Char char => ListLike text char =>
    Config
    -> kind (DocumentParsing text char m) Error value
    -> Stream m (Nontrivial text char)
    -> m (Either Error value)
parseOnly config p input =
    evalStateT (parse config input p) DocumentMemory.init
