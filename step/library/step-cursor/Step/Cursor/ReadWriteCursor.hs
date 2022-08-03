{-# language FlexibleInstances, FunctionalDependencies, GADTs, ViewPatterns, PatternSynonyms #-}

module Step.Cursor.ReadWriteCursor
  (
    ReadWriteCursor (..),
    rebaseCursor,
    lookAhead_,
    pattern Run, inp, com, runn,
  )
  where

import Step.Internal.Prelude

import Step.Cursor.Stream (Stream, streamRST)
import qualified Step.Cursor.Stream as Stream

import Step.Cursor.AdvanceResult (AdvanceResult)

import Step.RST

data ReadWriteCursor xs x r s m =
  forall s'. ReadWriteCursor
    { init :: RST r s m s'
    , visibleStateLens :: Lens' s' s
    , input :: Stream r s' m xs x
    , commit :: Positive Natural -> RST r s' m AdvanceResult
    }

rebaseCursor :: Monad m1 => (forall a. m1 a -> m2 a) -> ReadWriteCursor xs x r s m1 -> ReadWriteCursor xs x r s m2
rebaseCursor o ReadWriteCursor{ init, commit, input, visibleStateLens } =
  ReadWriteCursor
    { init = hoist o init
    , visibleStateLens
    , commit = hoist o . commit
    , input = over streamRST (hoist o) input
    }

instance Contravariant (ReadWriteCursor xs x r s m) (ReadWriteCursor xs x r' s m) r r' where
    contramap f ReadWriteCursor{ init, commit, input, visibleStateLens } =
      ReadWriteCursor
        { init = contramap f init
        , visibleStateLens
        , input = contramap f input
        , commit = contramap f . commit
        }

pattern Run :: Monad m =>
  Stream r s' m xs x
  -> (Positive Natural -> RST r s' m AdvanceResult)
  -> (forall a. RST r s' m a -> RST r s m a)
  -> ReadWriteCursor xs x r s m
pattern Run{ inp, com, runn } <- (runCursor -> RunReadWriteCursor{ input' = inp, commit' = com, run = runn })

data RunReadWriteCursor xs x r s m =
    forall s'. RunReadWriteCursor
      { input' :: Stream r s' m xs x
      , commit' :: Positive Natural -> RST r s' m AdvanceResult
      , run :: forall a. RST r s' m a -> RST r s m a
      }

-- | Looks at some input and immediately discards it
--
-- For a buffering cursor, this ensures that there is something buffered.
--
lookAhead_ :: Monad m => ReadWriteCursor xs x r s m -> RST r s m ()
lookAhead_ (runCursor -> RunReadWriteCursor{ input', run }) =
    run (void (Stream.next input'))

runCursor :: Monad m => ReadWriteCursor xs x r s m -> RunReadWriteCursor xs x r s m
runCursor ReadWriteCursor{ init, visibleStateLens, input, commit } =
  RunReadWriteCursor
    { input' = input
    , commit' = commit
    , run = \a -> do
        r <- ask
        s <- init
        (x, s') <- lift (runRST a r s)
        put (view visibleStateLens s')
        return x
    }
