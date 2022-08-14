{-# language DataKinds, FlexibleContexts, FlexibleInstances, KindSignatures, StandaloneKindSignatures #-}
{-# language DerivingVia, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# language FlexibleContexts, FlexibleInstances, FunctionalDependencies, ViewPatterns #-}

module Step.Document.Base
  (
    -- * Types
    -- DocumentParsing,
    Config (..), Context (..), DocError (..), documentCursor, cursorPositionLens, lineHistoryLens, DocumentMemory (..),
    -- * Running parsers
    parse, parseOnly, parseSimple,
    configContextLens, ctxConfigLens,
    Parser,
  )
  where

import Step.Internal.Prelude

import Step.Action
import qualified Step.Action as Action

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

import Step.RunAction

import Text (Text)

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

data DocError = DocError{ errorContext :: ContextStack }
    deriving stock (Eq, Show)

---

data Context xs x s m =
  Context
    { ctxConfig :: Config x
    , ctxStream :: Stream () s m xs x
    }

ctxConfigLens :: Lens (Context xs x s m) (Context xs x s m) (Config x) (Config x)
ctxConfigLens = lens ctxConfig \x y -> x{ ctxConfig = y }

ctxStreamLens :: Lens
  (Context xs1 x s1 m1)
  (Context xs2 x s2 m2)
  (Stream () s1 m1 xs1 x)
  (Stream () s2 m2 xs2 x)
ctxStreamLens = lens ctxStream \x y -> x{ ctxStream = y }

configLineTerminatorsLens :: Lens
  (Config x1)
  (Config x2)
  (Lines.Terminators x1)
  (Lines.Terminators x2)
configLineTerminatorsLens = lens configLineTerminators \x y -> x{ configLineTerminators = y }

documentCursor :: Monad m =>
    DropOperation xs x -> SpanOperation xs x
    -> CursorRW xs x (Context xs x s m) (DocumentMemory xs x s) (Buffer xs x) m
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
    act xs x (Context xs x s m) (RST (Context xs x s m) (DocumentMemory xs x s) m) a
    -> Context xs x s m
    -> StateT (DocumentMemory xs x s) m (Either DocError a)
parse a =
  let
    -- c :: CursorRW' xs (Item xs) (Context xs (Item xs) s m) (DocumentMemory xs (Item xs) s) m
    c =
        -- CursorRW' $
        documentCursor LL.dropOperation LL.spanOperation
  in
    view rstState $ runAny (cursorRunRW c) (Action.castTo @Any a) <&> \case
          Right x -> Right x
          Left c' -> Left DocError{ errorContext = c' & view (ctxConfigLens % configContextLens) }

parseOnly :: (Is act Any, Monad m, ListLike xs x) =>
    act xs x (Context xs x s m) (RST (Context xs x s m) (DocumentMemory xs x s) m) a
    -> Context xs x s m
    -> s
    -> m (Either DocError a)
parseOnly p c s =
    evalStateT (parse p c) DocumentMemory{ buffer = [], lineHistory = Lines.empty, cursorPosition = 0, streamState = s }

type Parser (act :: Action) a = act
    Text Char
    (Context Text Char [Nontrivial Text Char] Identity)
    (RST ((Context Text Char [Nontrivial Text Char] Identity)) (DocumentMemory Text Char [Nontrivial Text Char]) Identity)
    a

parseSimple :: forall a act. (Is act Any) =>
    Parser act a
    -> [Nontrivial Text Char]
    -> Either DocError a
parseSimple p i = runIdentity (parseOnly p (Context def Cursor.list) i)
