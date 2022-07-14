{-# language FlexibleContexts, OverloadedStrings, QualifiedDo, RankNTypes, TypeFamilies #-}

-- | This module is just here to demonstrate that we can do everything that "Data.Attoparsec.Text" does.

module Step.Competitors.Attoparsec.Text where

import Step.Internal.Prelude

import Step.ActionTypes

import qualified Step.ActionTypes.Do as P
import qualified Step.Document.Prelude as P
import qualified Step.Document.Parser as P
import qualified Step.ActionTypes as P

import Step.Document.Config (Config)
import Step.Document.Error (Error)
import Step.DocumentMemory.Base (DocumentMemory)

import Char (Char)
import qualified Char

import qualified Text

import qualified Step.LookAhead.Action as LookAhead.Action

type Parser base kind value = kind Error (ReaderT Config (StateT (DocumentMemory Text base) base)) value

char :: Monad m => Char -> Parser m AtomicMove Char
char x = P.satisfy (== x) P.<?> "char " <> Text.pack (show x)

anyChar :: Monad m => Parser m AtomicMove Char
anyChar = P.char P.<?> "anyChar"

notChar :: Monad m => Char -> Parser m AtomicMove Char
notChar x = P.satisfy (/= x) P.<?> "not " <> Text.singleton x

satisfy :: Monad m => (Char -> Bool) -> Parser m AtomicMove Char
satisfy f = P.satisfy f P.<?> "satisfy"

satisfyWith :: Monad m => (Char -> a) -> (a -> Bool) -> Parser m AtomicMove a
satisfyWith f ok = P.satisfyJust ((\x -> if ok x then Just x else Nothing) . f) P.<?> "satisfyWith"

skip :: Monad m => (Char -> Bool) -> Parser m AtomicMove ()
skip f = void (P.satisfy f) P.<?> "skip"

peekChar :: Monad m => Parser m SureQuery (Maybe Char)
peekChar = LookAhead.Action.next

peekChar' :: Monad m => Parser m Query Char
peekChar' = P.peekChar P.<?> "peekChar'"

digit :: Monad m => Parser m AtomicMove Char
digit = P.satisfy Char.isDigit P.<?> "digit"

letter :: Monad m => Parser m AtomicMove Char
letter = P.satisfy Char.isAlpha P.<?> "letter"

-- todo
-- space :: Parser Char

-- todo
-- inClass :: String -> Char -> Bool

-- todo
-- notInClass :: String -> Char -> Bool

-- todo
-- string :: Text -> Parser Text

-- todo
-- asciiCI :: Text -> Parser Text

-- todo
-- skipSpace :: Parser ()

-- todo
-- skipWhile :: (Char -> Bool) -> Parser ()

-- todo
-- scan :: s -> (s -> Char -> Maybe s) -> Parser Text

-- todo
-- runScanner :: s -> (s -> Char -> Maybe s) -> Parser (Text, s)

-- todo
-- take :: Natural -> Parser Text

-- todo
-- takeWhile :: (Char -> Bool) -> Parser Text

-- todo
-- takeWhile1 :: (Char -> Bool) -> Parser Text

-- todo
-- takeTill :: (Char -> Bool) -> Parser Text

takeText :: Monad m => Parser m Sure Text
takeText = P.all

-- todo
-- takeLazyText :: Parser Text

-- todo
-- endOfLine :: Parser ()

-- todo
-- decimal :: Integral a => Parser a

-- todo
-- hexadecimal :: (Integral a, Bits a) => Parser a

-- todo
-- signed :: Num a => Parser a -> Parser a

-- todo
-- double :: Parser Double

-- todo
-- rational :: Fractional a => Parser a

-- todo
-- scientific :: Parser Scientific

-- todo
-- try :: Parser a -> Parser a

-- todo
-- infix 0 <?>
-- (<?>) :: Parser a -> String -> Parser a

-- todo
-- choice :: ListLike list (Parser a) => list -> Parser a

count ::
    Monad m =>
    Loop0 k k' =>
    Natural -> Parser m k a -> Parser m k' [a]
count = P.count0

option :: Monad m => Atomic k1 k2 =>
    a -> Parser m k1 a -> Parser m k2 a
option b p = fromMaybe b P.<$> P.try p

many, many' :: Monad m => Parser m AtomicMove a -> Parser m Sure [a]
many = P.repetition0
many' p = many P.do{ x <- p; P.return $! x }

many1, many1' :: Monad m => Parser m AtomicMove a -> Parser m AtomicMove (NonEmpty a)
many1 = P.repetition1
many1' p = many1 P.do{ x <- p; P.return $! x }

-- todo
-- manyTill, manyTill' :: ListLike list a => Parser a -> Parser b -> Parser list

-- todo
-- sepBy, sepBy' :: ListLike list a => Parser a -> Parser s -> Parser list

-- todo
-- sepBy1, sepBy1' :: ListLike list a => Parser a -> Parser s -> Parser list

-- todo
-- skipMany :: Parser a -> Parser ()

-- todo
-- skipMany1 :: Parser a -> Parser ()

-- todo
-- eitherP :: Parser a -> Parser b -> Parser (Either a b)

-- todo
-- match :: Parser a -> Parser (Text, a)

endOfInput :: Monad m => Parser m Query ()
endOfInput = P.end P.<?> "endOfInput"

atEnd :: Monad m => Parser m SureQuery Bool
atEnd = LookAhead.Action.atEnd
