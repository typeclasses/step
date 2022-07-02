module Step.Buffer.State where

import Step.Internal.Prelude

import Step.Buffer.Base (Buffer)
import qualified Step.Buffer.Base as Buffer

import Step.Nontrivial.Base (Nontrivial)
import qualified Step.Nontrivial.Base as Nontrivial

takeChar :: (Monad m, ListLike chunk char) => StateT (Buffer chunk) m (Maybe char)
takeChar = do
    b <- get
    case Buffer.unconsChar b of
        Nothing -> return Nothing
        Just (c, b') -> do
            put b'
            return (Just c)

takeChunk :: (Monad m, ListLike chunk char) => StateT (Buffer chunk) m (Maybe (Nontrivial chunk))
takeChunk = do
    b <- get
    case Buffer.unconsChunk b of
        Nothing -> return Nothing
        Just (c, b') -> do
            put b'
            return (Just c)

data TakeStringResult chunk =
    TakeStringFail
  | TakeStringPartial (Nontrivial chunk) -- ^ What further needed text remains
  | TakeStringSuccess

takeString :: (Monad m, ListLike chunk char, Eq chunk, Eq char) =>
    chunk -> StateT (Buffer chunk) m (TakeStringResult chunk)
takeString x = case Nontrivial.refine x of Nothing -> return TakeStringSuccess; Just y -> takeNontrivialString y

takeNontrivialString :: (Monad m, ListLike chunk char, Eq chunk, Eq char) =>
    Nontrivial chunk -> StateT (Buffer chunk) m (TakeStringResult chunk)
takeNontrivialString c = do
    b <- get
    case Buffer.stripNontrivialPrefix c b of
        Buffer.StripPrefixFail -> return TakeStringFail
        Buffer.StripPrefixPartial c' -> do
            put Buffer.empty
            return (TakeStringPartial c')
        Buffer.StripPrefixSuccess b' -> do
            put b'
            return TakeStringSuccess
