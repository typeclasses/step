-- | This module is just here to demonstrate that we can do everything that "Data.Attoparsec.Text" does.
--
module Step.Document.Attoparsec where

import Step.Internal.Prelude

import Step.Document.Parser

import Step.Action.SeparateTypes

import qualified Step.Document.Prelude as P

char :: ListLike text char => Monad m => Eq char => char -> Parser text MoveUndo m char
char x = P.satisfy (== x)

anyChar :: ListLike text char => Monad m => Parser text MoveUndo m char
anyChar = P.char

notChar :: ListLike text char => Eq char => Monad m => char -> Parser text MoveUndo m char
notChar x = P.satisfy (/= x)

satisfy :: Monad m => ListLike text char => (char -> Bool) -> Parser text MoveUndo m char
satisfy = P.satisfy

satisfyWith :: Monad m => ListLike text char => (char -> a) -> (a -> Bool) -> Parser text MoveUndo m a
satisfyWith f ok = P.satisfyJust ((\x -> if ok x then Just x else Nothing) . f)

skip :: ListLike text char => Monad m => (char -> Bool) -> Parser text MoveUndo m ()
skip f = void (P.satisfy f)

peekChar :: ListLike text char => Monad m => Parser text SureStatic m (Maybe char)
peekChar = P.peekCharMaybe

-- peekChar' :: Parser Char

-- digit :: Parser Char

-- letter :: Parser Char

-- space :: Parser Char

-- inClass :: String -> Char -> Bool

-- notInClass :: String -> Char -> Bool

-- string :: Text -> Parser Text

-- asciiCI :: Text -> Parser Text

-- skipSpace :: Parser ()

-- skipWhile :: (Char -> Bool) -> Parser ()

-- scan :: s -> (s -> Char -> Maybe s) -> Parser Text

-- runScanner :: s -> (s -> Char -> Maybe s) -> Parser (Text, s)

-- take :: Natural -> Parser Text

-- takeWhile :: (Char -> Bool) -> Parser Text

-- takeWhile1 :: (Char -> Bool) -> Parser Text

-- takeTill :: (Char -> Bool) -> Parser Text

-- takeText :: Parser Text

-- takeLazyText :: Parser Text

-- endOfLine :: Parser ()

-- decimal :: Integral a => Parser a

-- hexadecimal :: (Integral a, Bits a) => Parser a

-- signed :: Num a => Parser a -> Parser a

-- double :: Parser Double

-- rational :: Fractional a => Parser a

-- scientific :: Parser Scientific

-- try :: Parser a -> Parser a

-- infix 0 <?>
-- (<?>) :: Parser a -> String -> Parser a

-- choice :: ListLike list (Parser a) => list -> Parser a

-- count :: ListLike list a => Natural -> Parser a -> Parser list

-- option :: a -> Parser a -> Parser a

-- -- | 0 or more
-- many, many' :: ListLike list a => Parser a -> Parser list

-- many1, many1' :: ListLike list a => Parser a -> Parser (Nontrivial list)

-- manyTill, manyTill' :: ListLike list a => Parser a -> Parser b -> Parser list

-- sepBy, sepBy' :: ListLike list a => Parser a -> Parser s -> Parser list

-- sepBy1, sepBy1' :: ListLike list a => Parser a -> Parser s -> Parser list

-- skipMany :: Parser a -> Parser ()

-- skipMany1 :: Parser a -> Parser ()

-- eitherP :: Parser a -> Parser b -> Parser (Either a b)

-- match :: Parser a -> Parser (Text, a)

-- endOfInput :: Parser ()

-- atEnd :: Parser Bool
