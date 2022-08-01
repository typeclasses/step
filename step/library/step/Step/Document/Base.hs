{-# language DataKinds, FlexibleContexts, FlexibleInstances, KindSignatures, StandaloneKindSignatures #-}
{-# language DerivingVia, GeneralizedNewtypeDeriving, StandaloneDeriving #-}

module Step.Document.Base
  (
    -- * Types
    -- DocumentParsing,
    Config (..), Context (..), Error (..), documentCursor,
    -- * Running parsers
    -- parse, parseOnly,
  )
  where

import Step.Internal.Prelude

import Step.Document.Memory (DocumentMemory)
import qualified Step.Document.Memory as DocumentMemory
import qualified Step.Document.Memory as DM

import Step.Document.Lines (Char)

import qualified Step.ActionTypes as Action
import Step.ActionTypes.Unsafe (Any (Any))

import Text (Text)

import Step.Cursor (Cursory, curse, Stream, ReadWriteCursor (..), streamRST)
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
import qualified Step.Document.Lines as Lines

import Step.Buffer.Buffer

import Step.RST

import Optics

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

data Error = Error{ errorContext :: [Text] }
    deriving stock (Eq, Show)

---

data Context xs x s m =
  Context
    { ctxConfig :: Config
    , ctxStream :: Stream () s m xs x
    }

-- ctxConfigLens = lens ctxConfig \x y -> x{ ctxConfig = y }

-- ctxStreamLens = lens ctxStream \x y -> x{ ctxStream = y }

---

-- newtype DocumentParsing xs x s m a =
--     DocumentParsing (RST (Context xs x s m) (DocumentMemory xs x) m a)
--     deriving newtype (Functor, Applicative, Monad)

-- instance (Monad m, ListLike xs x) => Cursory (DocumentParsing xs x m) where
--     type CursoryText (DocumentParsing xs x m) = xs
--     type CursoryChar (DocumentParsing xs x m) = x
--     type CursoryBase (DocumentParsing xs x m) = m
--     type CursoryParam (DocumentParsing xs x m) = Context xs x m
--     type CursoryInternalState (DocumentParsing xs x m) = DocumentMemory xs x DoubleBuffer
--     type CursoryState (DocumentParsing xs x m) = DocumentMemory xs x Buffer
--     curse = documentCursor

documentCursor :: forall m xs x s. Monad m => ListLike xs x => Lines.Char x =>
    ReadWriteCursor xs x (Context xs x s m) (DocumentMemory xs x s) m
documentCursor =
    loadingCursor @(DocumentMemory xs x s) DM.bufferLens
        & countingCursor @(DocumentMemory xs x s) DM.cursorPositionLens
        & contramap
            (
              Cursor.record @(DocumentMemory xs x s)
                  (stateRST . zoom DM.lineHistoryLens . Lines.recordNontrivial)
              . over streamRST (zoom DM.streamStateLens)
              . ctxStream
            )

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

-- parse :: Monad m => Action.Is kind Any =>
--     Config -> kind (DocumentParsing xs x s m) Error a
--     -> StateT (DocumentMemory xs x) m (Either Error a)
-- parse config p =
--     _
    -- p & Action.cast @Any & \(Any (DocumentParsing p')) -> runReaderT p' config >>= \case
    --     Left (DocumentParsing errorMaker) -> Left <$> runReaderT errorMaker config
    --     Right x -> return (Right x)

-- parseOnly :: forall m xs x s kind value. Action.Is kind Any => Monad m => Char x => ListLike xs x =>
--     Config -> kind (DocumentParsing xs x s m) Error value -> Stream () () m xs x -> m (Either Error value)
-- parseOnly config p xs =
--     _
    -- evalStateT (parse config p) (DocumentMemory.fromStream xs)
