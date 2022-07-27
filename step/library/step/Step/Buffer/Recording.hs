{-# language DerivingVia, GeneralizedNewtypeDeriving #-}

module Step.Buffer.Recording where

import Step.Internal.Prelude

import Step.Buffer.Buffer
import Step.Buffer.BufferState
import Step.Buffer.DoubleBuffer
import Step.Buffer.BufferResult

import qualified Step.Buffer.BufferState as BufferState

import Step.Cursor
import qualified Step.Cursor as Cursor

data BufferPlus r xs x = BufferPlus{ extra :: r, buffer :: Buffer xs x }

bufferPlusBuffer :: Lens (BufferPlus r xs1 x1) (BufferPlus r xs2 x2) (Buffer xs1 x1) (Buffer xs2 x2)
bufferPlusBuffer = lens buffer \x y -> x{ buffer = y }

newtype BufferPlusState r xs x m a =
    BufferPlusState (StateT (BufferPlus r xs x) m a)
    deriving newtype (Functor, Applicative, Monad, MonadState (BufferPlus r xs x), MonadTrans)

instance (ListLike xs x, Monad m) => Cursory (BufferPlusState r xs x m) where
    type CursoryText (BufferPlusState r xs x m) = xs
    type CursoryChar (BufferPlusState r xs x m) = x
    type CursoryContext (BufferPlusState r xs x m) = DoubleBufferPlusState r xs x m

    cursoryRun (DoubleBufferPlusState a) = BufferPlusState do
        bp :: BufferPlus r xs x <- get
        let dbp :: DoubleBufferPlus r xs x = newDoubleBufferPlus bp
        (x, dbp' :: DoubleBufferPlus r xs x) <- lift $ runStateT a $ dbp
        put BufferPlus{ extra = extra' dbp', buffer = view uncommitted (buffer' dbp') }
        return x

    cursoryCommit n =
        DoubleBufferPlusState $ zoom (doubleBufferPlusDoubleBuffer % uncommitted) $ runBufferState $ BufferState.dropN n

    cursoryInput =
        Cursor.stream $ DoubleBufferPlusState $ zoom (doubleBufferPlusDoubleBuffer % unseen) $ runBufferState takeChunk

data DoubleBufferPlus r xs x = DoubleBufferPlus{ extra' :: r, buffer' :: DoubleBuffer xs x }

doubleBufferPlusDoubleBuffer :: Lens (DoubleBufferPlus r xs1 x1) (DoubleBufferPlus r xs2 x2) (DoubleBuffer xs1 x1) (DoubleBuffer xs2 x2)
doubleBufferPlusDoubleBuffer = lens buffer' \x y -> x{ buffer' = y }

doubleBufferPlusExtra :: Lens (DoubleBufferPlus r1 xs x) (DoubleBufferPlus r2 xs x) r1 r2
doubleBufferPlusExtra = lens extra' \x y -> x{ extra' = y }

newDoubleBufferPlus :: BufferPlus r xs x -> DoubleBufferPlus r xs x
newDoubleBufferPlus bp = DoubleBufferPlus (extra bp) (newDoubleBuffer (buffer bp))

newtype DoubleBufferPlusState r xs x m a =
    DoubleBufferPlusState (StateT (DoubleBufferPlus r xs x) m a)
    deriving newtype (Functor, Applicative, Monad, MonadState (DoubleBufferPlus r xs x), MonadTrans)

newtype RecordingDoubleBufferPlusState r xs x m a =
    RecordingDoubleBufferPlusState (Stream (StateT r m) xs x -> DoubleBufferPlusState r xs x m a)
    deriving (Functor, Applicative, Monad)
      via
        (
          ReaderT (Stream (StateT r m) xs x)
            (DoubleBufferPlusState r xs x m)
        )

instance MonadTrans (RecordingDoubleBufferPlusState r xs x) where
    lift a = RecordingDoubleBufferPlusState \_ -> lift a

newtype Recording r xs x m a =
    Recording (Stream (StateT r m) xs x -> BufferPlusState r xs x m a)
  deriving (Functor, Applicative, Monad)
      via (ReaderT (Stream (StateT r m) xs x) (BufferPlusState r xs x m))

instance MonadTrans (Recording r xs x) where
    lift a = Recording \_ -> lift a

instance (Monad m, ListLike xs x) => Cursory (Recording r xs x m) where
    type CursoryText (Recording r xs x m) = xs
    type CursoryChar (Recording r xs x m) = x
    type CursoryContext (Recording r xs x m) = RecordingDoubleBufferPlusState r xs x m

    cursoryRun (RecordingDoubleBufferPlusState a) = Recording \upstream -> do
        let DoubleBufferPlusState dbps = a upstream
        bp :: BufferPlus r xs x <- get
        let dbp :: DoubleBufferPlus r xs x = DoubleBufferPlus (extra bp) (newDoubleBuffer (buffer bp))
        (x, dbp' :: DoubleBufferPlus r xs x) <- lift $ runStateT dbps $ dbp
        put BufferPlus{ extra = extra' dbp', buffer = view uncommitted (buffer' dbp') }
        return x

    cursoryInput = Cursor.streamChoice bufferedInput freshInput
      where
        bufferedInput =
            cursoryInput @(BufferPlusState r xs x m) & Cursor.rebaseStream \a ->
                RecordingDoubleBufferPlusState \_ -> a
        freshInput = Cursor.stream (bufferMore *> Cursor.next bufferedInput)

    cursoryCommit =
        \n -> commitBuffered n >>= \case
            r@AdvanceSuccess -> return r
            YouCanNotAdvance n' -> commitFresh n'
      where
        commitBuffered n = RecordingDoubleBufferPlusState \_ -> cursoryCommit @(BufferPlusState r xs x m) n
        commitFresh n = bufferMore *> commitBuffered n

bufferMore :: Monad m => RecordingDoubleBufferPlusState r xs x m BufferResult
bufferMore = RecordingDoubleBufferPlusState \upstream ->
    DoubleBufferPlusState $ zoom doubleBufferPlusExtra (Cursor.next upstream) >>= \case
        Nothing -> return NothingToBuffer
        Just x -> do
            modifying (doubleBufferPlusDoubleBuffer % uncommitted % chunks) (:|> x)
            modifying (doubleBufferPlusDoubleBuffer % unseen % chunks) (:|> x)
            return BufferedMore
