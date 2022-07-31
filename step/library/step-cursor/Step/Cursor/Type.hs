{-# language GADTs #-}

module Step.Cursor.Type
  (
    Cursor (..),
    rebaseCursor,
    contramapCursor,
  )
  where

import Step.Internal.Prelude

import Step.Cursor.Stream (Stream, streamRST, contramapStream)

import Step.Cursor.AdvanceResult (AdvanceResult)

import Step.RST

import Optics

data Cursor xs x r s m =
  forall s'. Cursor
    { init :: s -> s'
    , extract :: s' -> s -> s
    , input :: Stream r (s', s) m xs x
    , commit :: Positive Natural -> RST r (s', s) m AdvanceResult
    }

rebaseCursor :: Monad m1 => (forall a. m1 a -> m2 a) -> Cursor xs x r s m1 -> Cursor xs x r s m2
rebaseCursor o Cursor{ init, commit, input, extract } =
  Cursor
    { init = init
    , extract = extract
    , commit = hoist o . commit
    , input = over streamRST (hoist o) input
    }

contramapCursor :: forall xs x r r' s m. Monad m =>
    (r' -> r)
    -> Cursor xs x r s m
    -> Cursor xs x r' s m
contramapCursor f Cursor{ init, commit, input, extract } =
  Cursor
    { init = init
    , extract = extract
    , input = contramapStream f input
    , commit = contramapRST f . commit
    }
