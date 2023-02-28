module HTTP where

import Essentials
import Cursor.Interface

import Block (ASCII, ASCII1)
import Control.Monad.Except (ExceptT)
import Cursor.Reader.Type (ReaderPlus)
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.String (IsString)
import Data.Text (Text)
import Data.Word (Word8)
import Integer (Natural)

import qualified ASCII.Char as ASCII
import qualified Block
import qualified Cursor.Reader.Examples as Read
import qualified Cursor.Reader.Utilities as Read
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map

newtype Error = Error Text
  deriving newtype IsString

data Request = Request Start FieldMap (Maybe Body)

data Start = Start Method Target Version

data Version = Version Natural Natural

data Field = Field ASCII ASCII

data FieldMap = FieldMap (Map ASCII ASCII)

data Target = Target ASCII

data Body = Body ByteString

data Method = GET | POST | HEAD deriving stock (Eq, Ord, Show, Enum, Bounded)

readRequest :: ExceptT Error (ReaderPlus up action 'Write Word8 ByteString) Request
readRequest = do
    (start, fields) <- withByteStringAsAscii do
        start <- readStart
        fields <- makeFieldMap <$> readFieldList
        readBlankLine
        pure (start, fields)
    body <- readBody fields
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
readStart = _

readMethod :: ExceptT Error (ReaderPlus up action 'Write ASCII.Char ASCII1) Method
readMethod = Read.just "method" Read.enum

readSpace :: ExceptT Error (ReaderPlus up action 'Write ASCII.Char ASCII1) ()
readSpace = Read.true "space" $ Read.exact " "

readBlankLine :: ExceptT Error (ReaderPlus up action 'Write item ASCII1) ()
readBlankLine = Read.true "blank line" $ Read.exact "\r\n"

readBody :: FieldMap -> ExceptT Error (ReaderPlus up action 'Write Word8 ByteString) (Maybe Body)
readBody fields = _
