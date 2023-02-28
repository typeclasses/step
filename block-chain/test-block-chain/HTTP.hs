module HTTP where

import Essentials
import Cursor.Interface

import Data.ByteString (ByteString)
import Cursor.Reader.Type (ReaderPlus)
import Cursor.Reader.Examples (enum, exact)
import Cursor.Reader.Utilities (true, just, repetition)
import Block (ASCII, ASCII1)
import Control.Monad.Except (ExceptT, throwError)
import Data.Text (Text)
import Integer (Natural)
import Data.String (IsString)
import Data.Word (Word8)

import qualified ASCII.Char as ASCII
import qualified Data.Sequence as Seq
import qualified Data.ByteString as LBS
import qualified Data.Map.Strict as Map

newtype Error = Error Text
  deriving newtype IsString

data Request = Request Start [Field] (Maybe Body)

data Start = Start Method Target Version

data Version = Version Natural Natural

data Field = Field ASCII ASCII

data Target = Target ASCII

data Body = Body ByteString

data Method = GET | POST | HEAD deriving stock (Eq, Ord, Show, Enum, Bounded)

readRequest :: ExceptT Error (ReaderPlus up action 'Write Word8 ByteString) Request
readRequest = do
    (start, fields) <- withByteStringAsAscii do
        start <- start
        fields <- repetitionExcept (Seq.singleton <$> readField)
        blankLine
        pure (start, fields)
    b <- readBody fields
    pure $ Request s fs b

readField :: ExceptT Error (ReaderPlus up action 'Write ASCII.Char ASCII1) (Maybe Field)
readField = _

readStart :: ExceptT Error (ReaderPlus up action 'Write ASCII.Char ASCII1) Start
readStart = _

readMethod :: ExceptT Error (ReaderPlus up action 'Write ASCII.Char ASCII1) Method
readMethod = just "method" enum

readSpace :: ExceptT Error (ReaderPlus up action 'Write ASCII.Char ASCII1) ()
readSpace = true "space" $ exact " "

readBody :: Map ASCII'lower ASCII -> ExceptT Error (ReaderPlus up action 'Write Word8 ByteString) ByteString
readBody fields = _
