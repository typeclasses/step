{-# language DataKinds, FlexibleContexts, FlexibleInstances, KindSignatures, StandaloneKindSignatures #-}
{-# language DerivingVia, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# language FlexibleContexts, FlexibleInstances, FunctionalDependencies, ViewPatterns #-}

module Step.Document.Base
  (
    -- * Types
    -- DocumentParsing,
    Config (..), Context (..), Error (..), documentCursor, cursorPositionLens, lineHistoryLens, DocumentMemory,
    -- * Running parsers
    parse, parseOnly, parseSimple,
    configContextLens, ctxConfigLens,
  )
  where

import Step.Internal.Prelude

import qualified Step.ActionTypes as Action
import Step.ActionTypes.Unsafe (Any (Any))

import Step.Cursor
import qualified Step.Cursor as Cursor

import Step.RST

import Step.Nontrivial (Nontrivial, DropOperation, SpanOperation)
import qualified Step.Nontrivial.ListLike as LL

import Char (Char)

import Step.LineHistory (LineHistory)
import qualified Step.LineHistory as Lines

import Step.Buffer

import Step.GeneralCursors

import Step.ContextStack

import Step.ActionTypes

---

data DocumentMemory xs x s =
  DocumentMemory
    { streamState :: s
    , buffer :: Buffer xs x
    , lineHistory :: LineHistory
    , cursorPosition :: CursorPosition
    }

lineHistoryLens = lens lineHistory \x y -> x{ lineHistory = y }

cursorPositionLens = lens cursorPosition \x y -> x{ cursorPosition = y }

bufferLens = lens buffer \x y -> x{ buffer = y }

streamStateLens = lens streamState \x y -> x{ streamState = y }

---

data Config x = Config{ configContext :: ContextStack, configLineTerminators :: Lines.Terminators x }

instance Default (Config Char)
  where
    def = Config{ configContext = ContextStack Empty, configLineTerminators = Lines.charTerminators }

configContextLens :: Lens (Config x) (Config x) ContextStack ContextStack
configContextLens = lens configContext \x y -> x{ configContext = y }

---

data Error = Error{ errorContext :: ContextStack }
    deriving stock (Eq, Show)

---

data Context xs x s m =
  Context
    { ctxConfig :: Config x
    , ctxStream :: Stream () s m xs x
    }

ctxConfigLens = lens ctxConfig \x y -> x{ ctxConfig = y }

ctxStreamLens = lens ctxStream \x y -> x{ ctxStream = y }

configLineTerminatorsLens = lens configLineTerminators \x y -> x{ configLineTerminators = y }

documentCursor :: Monad m =>
    DropOperation xs x -> SpanOperation xs x
    -> Cursor xs x (Context xs x s m) (DocumentMemory xs x s) m
documentCursor dropOp spanOp =
    loadingCursor dropOp bufferLens
        & contramap (recordingStream spanOp)
        & countingCursor cursorPositionLens

recordingStream :: Monad m =>
    SpanOperation xs x -> Context xs x s m -> Stream () (DocumentMemory xs x s) m xs x
recordingStream spanOp ctx =
    Cursor.record (record' spanOp ctx) $ over streamRST (zoom streamStateLens) $ ctxStream ctx

record' :: Monad m => SpanOperation xs x -> Context xs x s m -> Nontrivial xs x -> RST () (DocumentMemory xs x s) m ()
record' spanOp ctx = contraconst ts . zoom lineHistoryLens . (Lines.record spanOp)
  where
    ts = configLineTerminators (ctxConfig ctx)

parse :: forall act xs x s m a. (Is act Any, Monad m, ListLike xs x) =>
    act xs x (Context xs x s m) (DocumentMemory xs x s) m a
    -> Context xs x s m
    -> StateT (DocumentMemory xs x s) m (Either Error a)
parse (Action.cast -> Any p) =
  let
    c :: Cursor xs (Item xs) (Context xs (Item xs) s m) (DocumentMemory xs (Item xs) s) m
    c = documentCursor LL.dropOperation LL.spanOperation
  in
    view rstState $ p (Cursor.cursorRW c) <&> \case
          Right x -> Right x
          Left c' -> Left Error{ errorContext = c' & view (ctxConfigLens % configContextLens) }

parseOnly :: (Is act Any, Monad m, ListLike xs x) =>
    act xs x (Context xs x s m) (DocumentMemory xs x s) m a
    -> Context xs x s m
    -> s
    -> m (Either Error a)
parseOnly p c s =
    evalStateT (parse p c) DocumentMemory{ buffer = [], lineHistory = Lines.empty, cursorPosition = 0, streamState = s }

parseSimple :: forall xs a act. (Is act Any, ListLike xs Char) =>
    act xs Char (Context xs Char [Nontrivial xs Char] Identity) (DocumentMemory xs Char [Nontrivial xs Char]) Identity a
    -> [Nontrivial xs Char]
    -> Either Error a
parseSimple p i = runIdentity (parseOnly p (Context def Cursor.list) i)
