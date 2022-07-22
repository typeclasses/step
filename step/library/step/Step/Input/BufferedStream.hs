{-# language FlexibleContexts, FlexibleInstances, FunctionalDependencies, TypeFamilies #-}

module Step.Input.BufferedStream
  (
    {- * The type -} BufferedStream (..),
    {- * Constants -} empty,
    {- * Conversion with ListT -} toListT, fromListT,
    {- * Buffer querying -} bufferIsEmpty, isAllBuffered, bufferedHeadChar,
    {- * Buffer manipulation -} bufferUnconsChunk, bufferUnconsChar,
    {- * Taking by chunk -} takeChunk, considerChunk,
  )
  where

import Step.Internal.Prelude

import qualified ListT

import Step.Input.Buffer (Buffer)
import qualified Step.Input.Buffer as Buffer

import Step.Nontrivial.Base (Nontrivial)
import qualified Step.Nontrivial.Base as Nontrivial
import qualified Step.Nontrivial.ListT as Nontrivial.ListT
import qualified Step.Nontrivial.List as Nontrivial

import Step.Input.Cursor (Cursor (..))
import qualified Step.Input.Cursor as Cursor

import qualified Step.Input.AdvanceResult as Advance
import Step.Input.AdvanceResult (AdvanceResult)

import Step.Input.Buffering (Buffering (..))

import qualified Positive

import qualified Step.Nontrivial.SplitAtPositive as SplitAtPositive
import Step.Nontrivial.SplitAtPositive (splitAtPositive, SplitAtPositive)

---

data BufferedStream m text char =
  BufferedStream
    { buffer :: Buffer text char
    , pending :: Maybe (ListT m (Nontrivial text char))
        -- ^ 'Nothing' indicates that the end of the stream has been reached.
    }

instance (Monad m, ListLike text char) => Cursor (StateT (BufferedStream m text char) m) where
    type Text (StateT (BufferedStream m text char) m) = text
    type Char (StateT (BufferedStream m text char) m) = char
    curse = Cursor.stateSession takeChunk dropN

    -- forecast =
    --     changeBaseListT (zoom bufferLens) forecast
    --     <|>
    --     ListT
    --       (
    --         use pendingLens
    --         >>=
    --         maybe
    --             (return ListT.Nil)
    --             (
    --               fix \r p ->
    --                   lift (ListT.next p) >>= \case
    --                       ListT.Nil -> assign pendingLens Nothing $> ListT.Nil
    --                       ListT.Cons x xs -> do
    --                           modifying bufferLens (<> Buffer.singleton x)
    --                           assign pendingLens (Just xs)
    --                           return (ListT.Cons x (ListT (r xs)))
    --             )
    --       )
    -- advance n =
    --     zoom bufferLens (advance n) >>= \case
    --         Advance.Success -> return Advance.Success
    --         Advance.InsufficientInput n' -> use pendingLens >>= \case
    --             Nothing -> return (Advance.InsufficientInput n')
    --             Just p -> do
    --                 lift (ListT.next p) >>= \case
    --                     ListT.Nil -> assign pendingLens Nothing $> Advance.InsufficientInput n'
    --                     ListT.Cons x xs -> do
    --                         modifying bufferLens (<> Buffer.singleton x)
    --                         assign pendingLens (Just xs)
    --                         advance n'

instance (Monad m, ListLike text char) => Buffering (StateT (BufferedStream m text char) m) where

    fillBuffer1 = do
        ie <- get <&> Buffer.isEmpty . buffer
        when ie bufferMore

    bufferMore = (get <&> pending) >>= \case

        -- If the end of the stream has been reached, do nothing
        Nothing -> return ()

        Just p ->

            -- Perform the next step in the pending input stream
            lift (ListT.next p)

            >>= \case

                -- If the stream is now empty, change its value to 'Nothing' to remember that we have reached the end
                ListT.Nil -> assign pendingLens Nothing

                -- We got a new chunk of input.
                ListT.Cons x xs -> do

                    -- Add the chunk to the buffer
                    modifying bufferLens (<> Buffer.singleton x)

                    -- Remove the chunk from the pending input stream
                    assign pendingLens (Just xs)

---

bufferLens :: Lens' (BufferedStream m text char) (Buffer text char)
bufferLens = lens buffer \x y -> x{ buffer = y }

pendingLens :: Lens
    (BufferedStream m1 text char)
    (BufferedStream m2 text char)
    (Maybe (ListT m1 (Nontrivial text char)))
    (Maybe (ListT m2 (Nontrivial text char)))
pendingLens = lens pending \x y -> x{ pending = y }

empty :: BufferedStream m text char
empty = BufferedStream Buffer.empty Nothing

isAllBuffered :: BufferedStream m text char -> Bool
isAllBuffered = isNothing . pending

toListT :: Monad m => BufferedStream m text char -> ListT m (Nontrivial text char)
toListT x = Buffer.toListT (buffer x) <|> asum (pending x)

fromListT :: ListLike text char => Monad m => ListT m text -> BufferedStream m text char
fromListT x = BufferedStream{ buffer = Buffer.empty, pending = Just (Nontrivial.ListT.filter x) }

bufferIsEmpty :: BufferedStream m text char -> Bool
bufferIsEmpty = Buffer.isEmpty . buffer

bufferUnconsChunk :: BufferedStream m text char -> Maybe (Nontrivial text char, BufferedStream m text char)
bufferUnconsChunk s = case Buffer.unconsChunk (buffer s) of
    Nothing -> Nothing
    Just (c, b') -> Just (c, s{ buffer = b' })

bufferUnconsChar :: ListLike text char => BufferedStream m text char -> Maybe (char, BufferedStream m text char)
bufferUnconsChar s = do
    (c, b') <- Buffer.unconsChar (buffer s)
    Just (c, s{ buffer = b' })

bufferedHeadChar :: ListLike text char => BufferedStream m text char -> Maybe char
bufferedHeadChar = Buffer.headChar . buffer

-- | Remove some text from the buffered stream, buffering more first if necessary, returning 'Nothing' if the end of the stream has been reached
takeChunk :: (ListLike text char, Monad m) => StateT (BufferedStream m text char) m (Maybe (Nontrivial text char))
takeChunk = do
    fillBuffer1
    zoom bufferLens Buffer.takeChunk

considerChunk :: Monad m => ListLike text char =>
    (Nontrivial text char -> (Natural, a)) -> StateT (BufferedStream m text char) m (Maybe a)
considerChunk f = zoom bufferLens (Buffer.considerChunk f)

dropN :: (ListLike text char, Monad m) =>
    Positive Natural -> StateT (BufferedStream m text char) m AdvanceResult
dropN = fix \r n ->
    takeChunk >>= \case
        Nothing -> return Advance.InsufficientInput{ Advance.shortfall = n }
        Just x -> case splitAtPositive n x of
            SplitAtPositive.All -> return Advance.Success
            SplitAtPositive.Split _ b -> zoom bufferLens (Buffer.putNontrivialChunk b) $> Advance.Success
            SplitAtPositive.Insufficient n' -> r n'
