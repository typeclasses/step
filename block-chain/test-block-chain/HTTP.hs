module HTTP where

import Essentials
import Cursor.Interface

import Block (ASCII, ASCII1, ByteString1)
import Control.Monad.Except (ExceptT)
import Cursor.Morph (morphExcept, decodeAscii)
import Cursor.Reader.Type (ReaderPlus)
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.String (IsString, fromString)
import Data.Text (Text)
import Data.Word (Word8)
import Integer (Natural)
import Cursor (overExcept)

import qualified Cursor
import qualified ASCII.Char as ASCII
import qualified Block
import qualified Control.Monad.Except as Except
import qualified Cursor.Reader.Examples as Read
import qualified Cursor.Reader.Utilities as Read
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy as Text.Lazy
import qualified Optics
import qualified Data.Text.Lazy.Builder as Text (Builder)
import qualified Data.Text.Lazy.Builder as Text.Builder

data Error = Error [Text] Text

instance IsString Error where
    fromString = Error [] . fromString

context :: Monad m => Text -> ExceptT Error m a -> ExceptT Error m a
context t = Except.mapExceptT (<&> Optics.over Optics._Left (addContext t))

addContext :: Text -> Error -> Error
addContext t (Error ts x) = Error (t : ts) x

tbError :: Text.Builder -> Error
tbError = Error [] . Text.Lazy.toStrict . Text.Builder.toLazyText

data Request = Request Start FieldMap (Maybe Body)

data Start = Start Method Target Version

data Version = Version Natural Natural

data Field = Field ASCII ASCII

data FieldMap = FieldMap (Map ASCII ASCII)

data Target = Target ASCII

data Body = Body ByteString

data Method = GET | POST | HEAD deriving stock (Eq, Ord, Show, Enum, Bounded)

readRequest :: ExceptT Error (ReaderPlus up action 'Write Word8 ByteString1) Request
readRequest = do
    (start, fields) <- context "head" $ morphExcept decodeAscii do
        start <- context "start line" readStart
        fields <- context "fields" $ makeFieldMap <$> readFieldList
        context "end of fields" readBlankLine
        pure (start, fields)
    body <- context "body" $ readBody fields
    pure $ Request start fields body

readField :: ExceptT Error (ReaderPlus up action 'Write ASCII.Char ASCII1) (Maybe Field)
readField = _

readFieldList :: ExceptT Error (ReaderPlus up action 'Write ASCII.Char ASCII1) (Seq Field)
readFieldList =
    Read.whileJustExcept (fmap Seq.singleton <$> readField) <* readBlankLine

makeFieldMap :: Foldable list => list Field -> FieldMap
makeFieldMap = Foldable.toList >>> fmap (\(Field a b) -> (Block.asciiLower a, b))
    >>> Map.fromListWith (\a b -> a <> ", " <> b) >>> FieldMap

readStart :: ExceptT Error (ReaderPlus up action 'Write ASCII.Char ASCII1) Start
readStart = do
    method <- context "method" $ readMethod <* context "end" readSpace
    target <- context "target" $ readTarget <* context "end" readSpace
    version <- context "version" readVersion
    context "end" readEndOfLine
    pure $ Start method target version

readMethod :: ExceptT Error (ReaderPlus up action 'Write ASCII.Char ASCII1) Method
readMethod = Cursor.enumExceptText tbError

readTarget :: ExceptT Error (ReaderPlus up action 'Write ASCII.Char ASCII1) Target
readTarget = Cursor.takeWhile (/= ASCII.Space)

readVersion :: ExceptT Error (ReaderPlus up action 'Write ASCII.Char ASCII1) Version
readVersion = _

readSpace :: ExceptT Error (ReaderPlus up action 'Write ASCII.Char ASCII1) ()
readSpace = Read.true "expected space" $ Read.exact " "

readEndOfLine :: ExceptT Error (ReaderPlus up action 'Write item ASCII1) ()
readEndOfLine = Read.true "expected line terminator" $ Read.exact "\r\n"

readBlankLine :: ExceptT Error (ReaderPlus up action 'Write item ASCII1) ()
readBlankLine = Read.true "expected blank line" $ Read.exact "\r\n"

readBody :: FieldMap -> ExceptT Error (ReaderPlus up action 'Write Word8 ByteString1) (Maybe Body)
readBody fields = _
