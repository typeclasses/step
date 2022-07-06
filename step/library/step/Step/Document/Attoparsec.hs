-- | This module is just here to demonstrate that we can do everything that "Data.Attoparsec.Text" does.
--
module Step.Document.Attoparsec where

import Step.Internal.Prelude

import Step.Document.Parser

import Step.Action.Safe

import qualified Step.Document.Do as P
import qualified Step.Document.Prelude as P

char :: ListLike text char => Monad m => Eq char => char -> Parser text MoveAtom m char
char x = P.satisfy (== x)

anyChar :: ListLike text char => Monad m => Parser text MoveAtom m char
anyChar = P.char

notChar :: ListLike text char => Eq char => Monad m => char -> Parser text MoveAtom m char
notChar x = P.satisfy (/= x)

satisfy :: Monad m => ListLike text char => (char -> Bool) -> Parser text MoveAtom m char
satisfy = P.satisfy

satisfyWith :: Monad m => ListLike text char => (char -> a) -> (a -> Bool) -> Parser text MoveAtom m a
satisfyWith f ok = P.satisfyJust ((\x -> if ok x then Just x else Nothing) . f)

skip :: ListLike text char => Monad m => (char -> Bool) -> Parser text MoveAtom m ()
skip f = void (P.satisfy f)

peekChar :: ListLike text char => Monad m => Parser text SureStatic m (Maybe char)
peekChar = P.peekCharMaybe

peekChar' :: Monad m => ListLike text char => Parser text Static m char
peekChar' = P.peekChar

-- todo
-- digit :: Parser Char

-- todo
-- letter :: Parser Char

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

takeText :: Monad m => ListLike text char => Parser text Sure m text
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
    Natural -> Parser text k m a -> Parser text k' m [a]
count = P.count0

option :: Monad m => IsAction (Try k) => Atomic k =>
    a -> Parser text k m a -> Parser text (Try k) m a
option b p = fromMaybe b P.<$> P.try p

many, many' :: Monad m => Parser text MoveAtom m a -> Parser text Sure m [a]
many = P.repetition0
many' p = many P.do{ x <- p; P.return $! x }

many1, many1' :: Monad m => Parser text MoveAtom m a -> Parser text Move m (NonEmpty a)
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

endOfInput :: Monad m => ListLike text char => Parser text Static m ()
endOfInput = P.end

atEnd :: Monad m => ListLike text char => Parser text SureStatic m Bool
atEnd = P.atEnd
