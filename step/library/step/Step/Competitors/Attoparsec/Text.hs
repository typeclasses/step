{-# language DataKinds, KindSignatures, FlexibleContexts, OverloadedStrings, QualifiedDo #-}

-- | This module is just here to demonstrate that we can do everything that "Data.Attoparsec.Text" does.

module Step.Competitors.Attoparsec.Text where

import Step.Internal.Prelude

import Step.ActionTypes
import qualified Step.ActionTypes as A

import qualified Step.Document as P
import Step.Document (Error)

import Char (Char)
import qualified Char

import qualified Text
import Text (Text)

import qualified Step.Nontrivial as Nontrivial

import qualified Step.Document as Doc

import qualified ListLike

import Step.ContextStack

import Step.Nontrivial

import Optics

type Parser s m (act :: Action) a =
    act Text Char
        (Doc.Context Text Char [Nontrivial Text Char] m)
        (Doc.DocumentMemory Text Char s)
        (Doc.Context Text Char [Nontrivial Text Char] m)
        m a

char :: Monad m => Char -> Parser s m AtomicMove Char
char x = P.satisfyJust (\y -> if y == x then Just y else Nothing) <?> "char " <> Text.pack (show x)

anyChar :: Monad m => Parser s m AtomicMove Char
anyChar = P.satisfyJust Just <?> "anyChar"

notChar :: Monad m => Char -> Parser s m AtomicMove Char
notChar x = P.satisfyJust (\y -> if y /= x then Just y else Nothing) <?> "not " <> Text.singleton x

satisfy :: Monad m => (Char -> Bool) -> Parser s m AtomicMove Char
satisfy f = P.satisfyJust (\x -> if f x then Just x else Nothing) <?> "satisfy"

satisfyWith :: Monad m => (Char -> a) -> (a -> Bool) -> Parser s m AtomicMove a
satisfyWith f ok = P.satisfyJust ((\x -> if ok x then Just x else Nothing) . f) <?> "satisfyWith"

skip :: Monad m => (Char -> Bool) -> Parser s m AtomicMove ()
skip f = P.satisfyJust (\x -> if f x then Just () else Nothing) <?> "skip"

peekChar :: Monad m => Parser s m SureQuery (Maybe Char)
peekChar = P.nextCharMaybe

peekChar' :: Monad m => Parser s m Query Char
peekChar' = (P.nextCharMaybe P.>>= maybe (cast @Query P.fail) return) <?> "peekChar'"

digit :: Monad m => Parser s m AtomicMove Char
digit = P.satisfyJust (\x -> if Char.isDigit x then Just x else Nothing) <?> "digit"

letter :: Monad m => Parser s m AtomicMove Char
letter = P.satisfyJust (\x -> if Char.isAlpha x then Just x else Nothing) <?> "letter"

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

takeText :: Monad m => Parser s m Sure Text
takeText = repetition0 P.takeNext <&> ListLike.foldMap Nontrivial.generalize

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

infix 0 <?>
(<?>) :: Monad m => ContravariantAction k => Parser s m k a -> Text -> Parser s m k a
p <?> c = P.contextualize c p

-- todo
-- choice :: ListLike list (Parser a) => list -> Parser a

count ::
    Monad m =>
    Loop0 k k' =>
    Natural -> Parser s m k a -> Parser s m k' [a]
count = P.count0

option :: Monad m => Atomic k1 k2 =>
    a -> Parser s m k1 a -> Parser s m k2 a
option b p = fromMaybe b P.<$> P.try p

many, many' :: Monad m => Parser s m AtomicMove a -> Parser s m Sure [a]
many = P.repetition0
many' p = many P.do{ x <- p; P.return $! x }

many1, many1' :: Monad m => Parser s m AtomicMove a -> Parser s m AtomicMove (NonEmpty a)
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

endOfInput :: Monad m => Parser s m Query ()
endOfInput = (P.atEnd P.>>= \case{ True -> cast (P.return ()); False -> cast @Query P.fail }) <?> "endOfInput"

atEnd :: Monad m => Parser s m SureQuery Bool
atEnd = P.atEnd
