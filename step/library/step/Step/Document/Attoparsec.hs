-- | This module is just here to demonstrate that we can do everything that "Data.Attoparsec.Text" does.
--
module Step.Document.Attoparsec where

import Step.Internal.Prelude

import Step.Document.Parser

import Step.Action.SeparateTypes
import Step.Action.UnifiedType (IsAction, (:>), ActionJoin)

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

-- todo
-- peekChar' :: Parser Char

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

-- todo
-- takeText :: Parser Text

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
    Monad m => IsAction k1 => IsAction k2 => ActionJoin k1 k2 =>
    MonadAction k2 => k1 :> k2 ~ k2 => ListLike list a =>
    Natural -> Parser text k1 m a -> Parser text k2 m list
count = P.count

-- todo
-- option :: a -> Parser a -> Parser a

-- todo
-- -- | 0 or more
-- many, many' :: ListLike list a => Parser a -> Parser list

-- todo
-- many1, many1' :: ListLike list a => Parser a -> Parser (Nontrivial list)

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

-- todo
-- endOfInput :: Parser ()

-- todo
-- atEnd :: Parser Bool
