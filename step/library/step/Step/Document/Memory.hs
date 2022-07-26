{-# language FlexibleContexts, FlexibleInstances, FunctionalDependencies #-}
{-# language DerivingVia, GeneralizedNewtypeDeriving #-}

module Step.Document.Memory
  (
    DocumentMemory,
    curse,
    cursorLocation,
    init,
  )
  where

import Step.Internal.Prelude

import Step.Document.Lines (LineHistory)
import qualified Step.Document.Lines as Lines

import Step.Input.Counter (Counter, Counting, cursorPosition)
import qualified Step.Input.Counter as Counter

import Step.Input.Cursor (Session (..))
import qualified Step.Input.Cursor as Cursor

import Step.Document.Locating (Locating)
import qualified Step.Document.Locating as Locating

import Step.Input.Stream (Stream (Stream))
import qualified Step.Input.Stream as Stream

import Step.Input.CursorPosition (CursorPosition)
import qualified Step.Input.CursorPosition as CursorPosition

import Step.Buffer.Base (Buffer)
import qualified Step.Buffer.Base as Buffer

import Step.Buffer.Session (BufferSession)
import qualified Step.Buffer.Session as BufferSession

import Step.Buffer.Result

import Step.Nontrivial (Nontrivial)
import qualified Step.Nontrivial as Nontrivial

import qualified Step.Input.AdvanceResult as Advance
import Step.Input.AdvanceResult (AdvanceResult, shortfall)

import Loc (Loc)

import qualified Positive

data DocumentMemory xs x =
  DocumentMemory
    { content :: LineHistory
    , position :: CursorPosition
    , buffer :: Buffer xs x
    }

init :: DocumentMemory text char
init = DocumentMemory Lines.empty CursorPosition.origin Buffer.empty

contentLens :: Lens (DocumentMemory xs x) (DocumentMemory xs x) LineHistory LineHistory
contentLens = lens content \x y -> x{ content = y }

positionLens :: Lens (DocumentMemory xs x) (DocumentMemory xs x) CursorPosition CursorPosition
positionLens = lens position \x y -> x{ position = y }

bufferLens :: Lens (DocumentMemory xs1 x1) (DocumentMemory xs2 x2) (Buffer xs1 x1) (Buffer xs2 x2)
bufferLens = lens buffer \x y -> x{ buffer = y }

data DocumentSession xs x =
  DocumentSession
    { sessionContent :: LineHistory
    , sessionPosition :: CursorPosition
    , sessionBuffer :: BufferSession xs x
    }

sessionContentLens :: Lens (DocumentSession xs x) (DocumentSession xs x) LineHistory LineHistory
sessionContentLens = lens sessionContent \x y -> x{ sessionContent = y }

sessionPositionLens :: Lens (DocumentSession xs x) (DocumentSession xs x) CursorPosition CursorPosition
sessionPositionLens = lens sessionPosition \x y -> x{ sessionPosition = y }

sessionBufferLens :: Lens (DocumentSession xs1 x1) (DocumentSession xs2 x2) (BufferSession xs1 x1) (BufferSession xs2 x2)
sessionBufferLens = lens sessionBuffer \x y -> x{ sessionBuffer = y }

newtype DocumentSessionM xs x m a = DocumentSessionM (Stream m (Nontrivial xs x) -> StateT (DocumentSession xs x) m a)
  deriving
    ( Functor, Applicative, Monad
    , MonadState (DocumentSession xs x)
    , MonadReader (Stream m (Nontrivial xs x))
    )
    via (ReaderT (Stream m (Nontrivial xs x)) (StateT (DocumentSession xs x) m))

getUpstream :: DocumentSessionM xs x m (Stream (DocumentSessionM xs x m) (Nontrivial xs x))
getUpstream = _

curse :: forall m xs x. Monad m => ListLike xs x => Lines.Char x => Session xs x (DocumentMemoryM xs x m)
curse = Session{ run, input, commit }
  where
    run :: forall a. DocumentSessionM xs x m a -> DocumentMemoryM xs x m a
    run (DocumentSessionM a) = DocumentMemoryM \up -> do
        ds :: DocumentSession xs x <- do
            dm <- get
            return DocumentSession{ sessionContent = content dm, sessionPosition = position dm, sessionBuffer = BufferSession.newBufferSession (buffer dm) }
        (x, bs') <- lift (runStateT (a up) ds)
        assign bufferLens (BufferSession.uncommitted (sessionBuffer bs'))
        return x

    input :: Stream (DocumentSessionM xs x m) (Nontrivial xs x)
    input = Stream $ do
        up <- getUpstream
        (DocumentSessionM \_ -> zoom (sessionBufferLens % BufferSession.unseenLens) Buffer.takeChunk) >>= \case
            Just x -> return (Just x)
            Nothing -> (DocumentSessionM \_ -> zoom sessionBufferLens (BufferSession.drink up)) >>= \case
                NothingToBuffer -> return Nothing
                BufferedMore -> DocumentSessionM \_ -> zoom (sessionBufferLens % BufferSession.unseenLens) Buffer.takeChunk

    commit :: Positive Natural -> DocumentSessionM xs x m AdvanceResult
    commit n = zoom BufferSession.uncommittedLens (Buffer.dropN n) >>= \case
        result@Advance.Success -> do
            lift $ modifying positionLens (CursorPosition.strictlyIncrease n)
            return result
        result@Advance.InsufficientInput{ Advance.shortfall = n' } -> do
            let progress :: Natural = review Positive.refine n - review Positive.refine n'
            lift $ modifying positionLens (CursorPosition.increase progress)
            BufferSession.drink upstream' >>= \case
                NothingToBuffer -> return result
                BufferedMore -> commit n'

newtype DocumentMemoryM text char m a =
    DocumentMemoryM (Stream m (Nontrivial text char) -> StateT (DocumentMemory text char) m a)
  deriving
    ( Functor, Applicative, Monad
    , MonadState (DocumentMemory text char)
    , MonadReader (Stream m (Nontrivial text char))
    )
    via (ReaderT (Stream m (Nontrivial text char)) (StateT (DocumentMemory text char) m))

-- newtype FromStream m text char m' a = FromStream (Stream m (Nontrivial text char) -> m' a)

instance (ListLike text char, Lines.Char char, Monad m) => Locating (DocumentMemoryM text char m)
  where
    position = attempt1
      where
        attempt1, attempt2 :: DocumentMemoryM text char m Loc

        attempt1 = use (to cursorLocation) >>= \case
            Lines.CursorAt x -> return x
            Lines.CursorLocationNeedsMoreInput -> drink *> attempt2

        attempt2 = use (to cursorLocation) <&> \case
            Lines.CursorAt x -> x
            Lines.CursorLocationNeedsMoreInput -> error "position @DocumentMemory: after buffering more, should not need more input to determine position"

instance Monad m => Counting (StateT (DocumentMemory text char) m) where
    cursorPosition = use positionLens

drink :: ListLike text char => Lines.Char char => Monad m => DocumentMemoryM text char m BufferResult
drink = DocumentMemoryM \xs -> lift (Stream.next xs) >>= \case
    Nothing -> do
        zoom contentLens Lines.terminate
        return NothingToBuffer
    Just x -> do
        zoom contentLens (Lines.recordNontrivial x)
        modifying bufferLens (Buffer.|> x)
        return BufferedMore

cursorLocation :: DocumentMemory text char -> Lines.CursorLocation
cursorLocation x =
    case Lines.locateCursorInDocument (position x) (content x) of
        Just cl -> cl
        Nothing -> error $ "invalid DocumentMemory, lh = " <> show (content x) <> " does not contain position " <> show (position x)
