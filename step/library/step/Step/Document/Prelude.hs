{-# language FlexibleContexts, QualifiedDo, TypeFamilies #-}

module Step.Document.Prelude
  (
  )
  where

import Step.Internal.Prelude hiding (while, under)

import Optics

import qualified NonEmpty

import qualified Step.Document.Parser as Parser
import Step.Document.Parser (DocumentParsing (DocumentParsing))

import qualified Loc
import Loc (Loc, SpanOrLoc)

import Step.DocumentMemory.Base (DocumentMemory)
import qualified Step.DocumentMemory.State as DocumentMemory.State

import qualified Step.Document.Config as Config

import qualified Step.ActionTypes.Do as P

import Step.ActionTypes (Atomic, Loop0, Loop1)
import qualified Step.ActionTypes as Action
import Step.ActionTypes.Types

import qualified Step.ActionTypes.Unsafe as Action.Unsafe

import Step.Extent.BufferedStream (Extent (Extent))

import Step.Document.Error (Error)

import Step.Document.Config (Config)

import qualified Step.Classes as Class
import qualified Step.Actions as Action

import Step.TakeOrLeave (TakeOrLeave (..))

-- within :: Monad m => ListLike text char => Action.Unsafe.CoerceAny act =>
--     Extent (StateT (DocumentMemory text m) m) text
--     -> Parser text m act a
--     -> Parser text m act a
-- within e = over o (DocumentMemory.State.within e)
--   where
--     o =
--         iso (\(Parser p) -> p) Parser
--         % iso (\(ActionReader f) -> f) ActionReader
--         % mapped
--         % Action.Unsafe.anyIsoUnsafe
--         % iso (\(Action.Unsafe.Any s) -> s) Action.Unsafe.Any

-- under :: Monad m => ListLike text char => Transform text m text -> Parser text m a -> Parser text m a
-- under (Transform t) (Parser p) = Parser \eo -> do
--     s <- get
--     (s', x) <- zoom ParseState.futureLens $ lift $ runStateT (p eo) _
--     _

-- match :: ListLike text char => Monad m => Extent text m -> Parser text m text
-- match (Extent e) = Parser \_eo -> do
--     s <- get
--     (s', t) <- lift $ execStateT (match' e) (s, ListLike.empty)
--     put s'
--     return (Right (ListLike.fold t))

-- match' :: Monad m => ListLike text char =>
--     ListT (StateT (Stream m text) m) text
--     -> StateT (ParseState text m, Seq text) m ()
-- match' e = do
--     step <- zoom (_1 % ParseState.futureLens) (ListT.next e)
--     case step of
--         ListT.Nil -> return ()
--         ListT.Cons x e' -> do
--             zoom _1 (ParseState.record x)
--             modifying _2 (`ListLike.snoc` x)
--             match' e'

-- while :: ListLike text char => Monad m => (Char -> Bool) -> Transform text m text
-- while f = Transform while'
--   where
--     while' = ListT do
--         cm <- Stream.State.takeChunk
--         case cm of
--             Nothing -> return ListT.Nil
--             Just c | ListLike.null c -> return (ListT.Cons c while')
--             Just c -> do
--                 let (a, b) = ListLike.span f c
--                 Stream.State.putChunk b
--                 return if ListLike.null a then ListT.Nil else ListT.Cons a while'
