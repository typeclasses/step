{-# language DataKinds, KindSignatures, FlexibleContexts, OverloadedStrings, QualifiedDo #-}

{-|

Description: An approximation of Attoparsec's Text interface

See "Data.Attoparsec.Text" in the @attoparsec@ package.

Attoparsec version: @0.14.4@

-}

module Step.Package.AttoparsecText where

import Essentials
import Step.Action.Types

import Data.Char (Char)
import Data.Text (Text)
import Chunk.Text (Text1)
import Text.Show (show)

import qualified Step.Action as A
import qualified Step.Package.General as A
import qualified Step.Do as A
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Chunk.Text as Text1
import qualified Block.Class as Chunk

newtype Trace = Trace [Text] deriving newtype (Eq, Ord, Show)

type Parser m (act :: Action) a = act Text1 m Trace a

infix 0 <?>
(<?>) :: IsAction act => Parser m act a -> Text -> Parser m act a
p <?> c = p & A.paramMap \(Trace cs) -> Trace (c : cs)

char :: Char -> Parser m Atom Char
char x = A.satisfyPredicate (== x) <?> ("char " <> Text.pack (show x))

anyChar :: Parser m Atom Char
anyChar = A.takeChar <?> "anyChar"

notChar :: Char -> Parser m Atom Char
notChar x = A.satisfyPredicate (/= x) <?> ("not " <> Text.singleton x)

satisfy :: (Char -> Bool) -> Parser m Atom Char
satisfy f = A.satisfyPredicate f <?> "satisfy"

satisfyWith :: (Char -> a) -> (a -> Bool) -> Parser m Atom a
satisfyWith f ok = A.satisfyJust ((\x -> if ok x then Just x else Nothing) . f) <?> "satisfyWith"

skip :: (Char -> Bool) -> Parser m Atom ()
skip f = void (A.satisfyPredicate f) <?> "skip"

peekChar :: Parser m SureQuery (Maybe Char)
peekChar = A.try A.peekChar

peekChar' :: Parser m Query Char
peekChar' = A.peekChar <?> "peekChar'"

digit :: Parser m Atom Char
digit = A.satisfyPredicate Char.isDigit <?> "digit"

letter :: Parser m Atom Char
letter = A.satisfyPredicate Char.isAlpha <?> "letter"

space :: Parser m Atom Char
space = A.satisfyPredicate Char.isSpace <?> "space"

string :: Text -> Parser m Atom ()
string x = case Chunk.refine x of
    Nothing -> A.pure' ()
    Just x' -> A.cast (A.takeParticularTextAtomic x') <?> "string"

asciiCI :: Text -> Parser m Atom Text
asciiCI x = case Chunk.refine x of
    Nothing -> A.pure' x
    Just x' -> A.cast $ A.takeMatchingTextAtomic Text1.asciiCI x' <&> Chunk.generalize

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

-- takeText :: Parser s m Sure Text
-- takeText = repetition0 P.takeNext <&> ListLike.foldMap Nontrivial.generalize

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
-- choice :: ListLike list (Parser a) => list -> Parser a

-- count ::
--     Monad m =>
--     Loop0 k k' =>
--     Natural -> Parser s m k a -> Parser s m k' [a]
-- count = P.count0

-- option :: Atomic k1 k2 =>
--     a -> Parser s m k1 a -> Parser s m k2 a
-- option b p = fromMaybe b P.<$> P.try p

-- many, many' :: Parser s m Atom a -> Parser s m Sure [a]
-- many = P.repetition0
-- many' p = many P.do{ x <- p; P.return $! x }

-- many1, many1' :: Parser s m Atom a -> Parser s m Atom (NonEmpty a)
-- many1 = P.repetition1
-- many1' p = many1 P.do{ x <- p; P.return $! x }

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

-- match :: Parser a -> Parser (Text, a)
-- match p = _

-- endOfInput :: Parser s m Query ()
-- endOfInput = (P.atEnd P.>>= \case{ True -> castTo @Query (P.return ()); False -> castTo @Query P.fail }) <?> "endOfInput"

-- atEnd :: Parser s m SureQuery Bool
-- atEnd = P.atEnd
