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

import Step.Document.Lines (Char)

import qualified Step.ActionTypes as Action
import Step.ActionTypes.Unsafe (Any (Any))

import Text (Text)

import Step.Cursor (Cursory, curse, Stream)
import qualified Step.Cursor as Cursor

import Step.Document.Locating (Locating (..))

import Step.Configuration (HasContextStack, contextStackLens, Configure, configure)
import qualified Step.Configuration as Config

import Step.Failure (Fallible)
import qualified Step.Failure as F

import Step.Input.Counter (Counting)
import qualified Step.Input.Counter as Counting

import Step.Input.Counter (cursorPosition)

import Step.Input.BufferedStream (BufferedStreamSession)

import Step.Input.CursorPosition (CursorPosition)

import Step.Document.Lines (LineHistory)

---

data Config = Config{ configContext :: [Text] }
    deriving stock (Eq, Show)

instance Default Config
  where
    def = Config{ configContext = [] }

instance HasContextStack Config where
    contextStackLens = lens configContext \x y -> x{ configContext = y }

---

data Error = Error{ errorContext :: [Text] }
    deriving stock (Eq, Show)

---

newtype DocumentParsing text char m a =
    DocumentParsing (ReaderT Config (StateT (DocumentMemory text char m) m) a)
    deriving (Functor, Applicative, Monad)
        via (ReaderT Config (StateT (DocumentMemory text char m) m))

instance (Monad m, ListLike xs x) => Cursory (DocumentParsing xs x m) where
    type CursoryText (DocumentParsing xs x m) = xs
    type CursoryChar (DocumentParsing xs x m) = x
    type CursoryContext (DocumentParsing xs x m) =
      (StateT CursorPosition
        (StateT (BufferedStreamSession
          (StateT LineHistory m) xs x)
            (StateT LineHistory m)))
    curse = Cursor.rebaseCursor (DocumentParsing . lift) DocumentMemory.curse

instance (ListLike text char, Monad m) => Locating (DocumentParsing text char m) where
    position = DocumentParsing position

instance (ListLike text char, Monad m) => Fallible (DocumentParsing text char m) where
    type Error (DocumentParsing text char m) = Error
    failure = DocumentParsing $ ReaderT \c -> return Error{ errorContext = configContext c }

instance (ListLike text char, Monad m) => Configure (DocumentParsing text char m) where
    type Config (DocumentParsing text char m) = Config
    configure f (DocumentParsing a) = DocumentParsing (configure f a)

instance (ListLike text char, Monad m) => Counting (DocumentParsing text char m) where
    cursorPosition = DocumentParsing Counting.cursorPosition

---

parse :: Monad m => Action.Is kind Any =>
    Config -> kind (DocumentParsing text char m) Error value
    -> StateT (DocumentMemory text char m) m (Either Error value)
parse config p =
    p & Action.cast @Any & \(Any (DocumentParsing p')) -> runReaderT p' config >>= \case
        Left (DocumentParsing errorMaker) -> Left <$> runReaderT errorMaker config
        Right x -> return (Right x)

parseOnly :: forall m xs x kind value. Action.Is kind Any => Monad m => Char x => ListLike xs x =>
    Config -> kind (DocumentParsing xs x m) Error value -> Stream m xs x -> m (Either Error value)
parseOnly config p xs =
    evalStateT (parse config p) (DocumentMemory.fromStream xs)
