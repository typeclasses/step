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

import Step.Cursor (Cursory, curse, Stream, Cursor (..))
import qualified Step.Cursor as Cursor

import Step.Document.Locating (Locating (..))

import Step.Configuration (HasContextStack, contextStackLens, Configure, configure)
import qualified Step.Configuration as Config

import Step.Failure (Fallible)
import qualified Step.Failure as F

import Step.Buffer.Counting
import Step.Buffer.Loading

import Step.Input.CursorPosition (CursorPosition)

import Step.Document.Lines (LineHistory)

import Step.Buffer.Buffer
import Step.Buffer.DoubleBuffer

import Step.RST

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

data Context xs x m = Context Config (Stream () LineHistory m xs x)

---

newtype DocumentParsing xs x m a =
    DocumentParsing (RST Config (DocumentMemory xs x Buffer) m a)
    deriving newtype (Functor, Applicative, Monad, MonadTrans, MFunctor)

-- instance (Monad m, ListLike xs x) => Cursory (DocumentParsing xs x m) where
--     type CursoryText (DocumentParsing xs x m) = xs
--     type CursoryChar (DocumentParsing xs x m) = x
--     type CursoryBase (DocumentParsing xs x m) = m
--     type CursoryParam (DocumentParsing xs x m) = Context xs x m
--     type CursoryInternalState (DocumentParsing xs x m) = DocumentMemory xs x DoubleBuffer
--     type CursoryState (DocumentParsing xs x m) = DocumentMemory xs x Buffer
--     curse = documentCursor

documentCursor :: Monad m => ListLike xs x =>
    Cursor xs x (Context xs x m) (DocumentMemory xs x DoubleBuffer) (DocumentMemory xs x Buffer) m
documentCursor = _

-- instance (ListLike text char, Monad m) => Locating (DocumentParsing text char m) where
--     position = DocumentParsing position

-- instance (ListLike text char, Monad m) => Fallible (DocumentParsing text char m) where
--     type Error (DocumentParsing text char m) = Error
--     failure = DocumentParsing $ ReaderT \c -> return Error{ errorContext = configContext c }

-- instance (ListLike text char, Monad m) => Configure (DocumentParsing text char m) where
--     type Config (DocumentParsing text char m) = Config
--     configure f (DocumentParsing a) = DocumentParsing (configure f a)

-- instance (ListLike text char, Monad m) => Counting (DocumentParsing text char m) where
--     cursorPosition = DocumentParsing Counting.cursorPosition

---

parse :: Monad m => Action.Is kind Any =>
    Config -> kind (DocumentParsing xs x m) Error a
    -> StateT (DocumentMemory xs x Buffer) m (Either Error a)
parse config p =
    _
    -- p & Action.cast @Any & \(Any (DocumentParsing p')) -> runReaderT p' config >>= \case
    --     Left (DocumentParsing errorMaker) -> Left <$> runReaderT errorMaker config
    --     Right x -> return (Right x)

parseOnly :: forall m xs x kind value. Action.Is kind Any => Monad m => Char x => ListLike xs x =>
    Config -> kind (DocumentParsing xs x m) Error value -> Stream () () m xs x -> m (Either Error value)
parseOnly config p xs =
    _
    -- evalStateT (parse config p) (DocumentMemory.fromStream xs)
