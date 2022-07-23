{-# language FlexibleContexts, OverloadedStrings, QualifiedDo #-}

-- | This module is just here to demonstrate that we can do everything that "Data.Attoparsec.Text" does.

module Step.Competitors.Attoparsec.Text where

import Step.Internal.Prelude

import Step.ActionTypes

import qualified Step.Document as P

import Char (Char)
import qualified Char

import qualified Text
import Text (Text)

import qualified Step.Nontrivial.Base as Nontrivial

type Parser base action value =
    action (P.DocumentParsing Text Char base) P.Error value

char :: Monad m => Char -> Parser m AtomicMove Char
char x = P.satisfyJust (\y -> if y == x then Just y else Nothing) P.<?> "char " <> Text.pack (show x)

anyChar :: Monad m => Parser m AtomicMove Char
anyChar = P.satisfyJust Just P.<?> "anyChar"

notChar :: Monad m => Char -> Parser m AtomicMove Char
notChar x = P.satisfyJust (\y -> if y /= x then Just y else Nothing) P.<?> "not " <> Text.singleton x

satisfy :: Monad m => (Char -> Bool) -> Parser m AtomicMove Char
satisfy f = P.satisfyJust (\x -> if f x then Just x else Nothing) P.<?> "satisfy"

satisfyWith :: Monad m => (Char -> a) -> (a -> Bool) -> Parser m AtomicMove a
satisfyWith f ok = P.satisfyJust ((\x -> if ok x then Just x else Nothing) . f) P.<?> "satisfyWith"

skip :: Monad m => (Char -> Bool) -> Parser m AtomicMove ()
skip f = P.satisfyJust (\x -> if f x then Just () else Nothing) P.<?> "skip"

peekChar :: Monad m => Parser m SureQuery (Maybe Char)
peekChar = P.peekCharMaybe

peekChar' :: Monad m => Parser m Query Char
peekChar' = (P.peekCharMaybe P.>>= maybe (cast P.failure) return) P.<?> "peekChar'"

digit :: Monad m => Parser m AtomicMove Char
digit = P.satisfyJust (\x -> if Char.isDigit x then Just x else Nothing) P.<?> "digit"

letter :: Monad m => Parser m AtomicMove Char
letter = P.satisfyJust (\x -> if Char.isAlpha x then Just x else Nothing) P.<?> "letter"

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
takeText = repetition0 P.some <&> Nontrivial.fold

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
endOfInput = (P.atEnd P.>>= \case{ True -> cast (P.return ()); False -> cast P.failure }) P.<?> "endOfInput"

atEnd :: Monad m => Parser m SureQuery Bool
atEnd = P.atEnd
