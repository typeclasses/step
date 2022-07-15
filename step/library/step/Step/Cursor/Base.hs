{-# language FlexibleContexts, FlexibleInstances, TypeFamilies #-}

module Step.Cursor.Base
  (
    {- * The type -} Cursor (..),
    {- * Optics -} positionLens, bufferedStreamLens,
    {- * Conversion with ListT -} fromListT, toListT,
  )
  where

import Step.Internal.Prelude

import Step.BufferedStream.Base (BufferedStream)
import qualified Step.BufferedStream.Base as BufferedStream

import Step.CursorPosition.Base (CursorPosition)

import Step.Nontrivial.Base (Nontrivial)

import Step.TakeOrLeave (TakeOrLeave (..))

import qualified Step.Classes.Base as Class

import qualified ListLike

---

data Cursor m text =
  Cursor
    { position :: CursorPosition
    , bufferedStream :: BufferedStream m text
    }

instance Monad m => Class.Peek1 (StateT (Cursor m text) m) where
    type Text (StateT (Cursor m text) m) = text
    peekCharMaybe = do
        modifyM fillBuffer1
        get <&> BufferedStream.bufferedHeadChar . bufferedStream
    atEnd = do
        modifyM fillBuffer1
        get <&> BufferedStream.bufferIsEmpty . bufferedStream

instance Monad m => Class.Take1 (StateT (Cursor m text) m) where
    considerChar f = StateT \cbs -> do
        cbs' <- fillBuffer1 cbs
        return case bufferUnconsChar cbs' of
            Nothing -> (Nothing, cbs')
            Just (x, cbs'') -> let r = f x in
                (Just r, case r of { Leave _ -> cbs'; Take _ -> cbs'' })
      where
        bufferUnconsChar cbs = do
            (c, b') <- BufferedStream.bufferUnconsChar (bufferedStream cbs)
            Just (c, Cursor{ bufferedStream = b', position = position cbs + 1 })

instance Monad m => Class.TakeAll (StateT (Cursor m text) m) where
    takeAll = do
        x <- zoom bufferedStreamLens Class.takeAll
        modifying positionLens (+ fromIntegral (ListLike.length x))
        return x

instance (Monad m, Eq text) => Class.SkipTextNonAtomic (StateT (Cursor m text) m) where
    skipTextNonAtomic x = do
        y <- zoom bufferedStreamLens (Class.skipTextNonAtomic x)
        modifying positionLens (+ fromIntegral (ListLike.length x))
        return y

---

positionLens :: Lens' (Cursor m text) CursorPosition
positionLens = lens position \x y -> x{ position = y }

bufferedStreamLens :: Lens' (Cursor m text) (BufferedStream m text)
bufferedStreamLens = lens bufferedStream \x y -> x{ bufferedStream = y }

---

fromListT :: Monad m => ListLike text char => ListT m text -> Cursor m text
fromListT xs = Cursor 0 (BufferedStream.fromListT xs)

toListT :: Monad m => Cursor m text -> ListT m (Nontrivial text)
toListT = BufferedStream.toListT . view bufferedStreamLens

---

fillBuffer1 :: Monad m => Cursor m text -> m (Cursor m text)
fillBuffer1 = traverseOf bufferedStreamLens BufferedStream.fillBuffer1
