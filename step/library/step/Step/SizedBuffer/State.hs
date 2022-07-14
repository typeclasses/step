module Step.SizedBuffer.State where

import Step.Internal.Prelude

import Step.SizedBuffer.Base (Buffer)
import qualified Step.SizedBuffer.Base as Buffer

import Step.Nontrivial.Base (Nontrivial)
import qualified Step.Nontrivial.Base as Nontrivial

takeChar :: (Monad m, ListLike text char) => StateT (Buffer text) m (Maybe char)
takeChar = do
    b <- get
    case Buffer.unconsChar b of
        Nothing -> return Nothing
        Just (c, b') -> do
            put b'
            return (Just c)

takeChunk :: (Monad m, ListLike text char) => StateT (Buffer text) m (Maybe (Nontrivial text))
takeChunk = do
    b <- get
    case Buffer.unconsChunk b of
        Nothing -> return Nothing
        Just (c, b') -> do
            put b'
            return (Just c)

data TakeStringResult text =
    TakeStringFail
  | TakeStringPartial (Nontrivial text) -- ^ What further needed text remains
  | TakeStringSuccess

takeString :: (Monad m, ListLike text char, Eq text, Eq char) =>
    text -> StateT (Buffer text) m (TakeStringResult text)
takeString x = case Nontrivial.refine x of Nothing -> return TakeStringSuccess; Just y -> takeNontrivialString y

takeNontrivialString :: (Monad m, ListLike text char, Eq text, Eq char) =>
    Nontrivial text -> StateT (Buffer text) m (TakeStringResult text)
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
