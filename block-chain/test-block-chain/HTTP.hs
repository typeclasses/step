module HTTP where

import Essentials
import Relude
import Cursor.Interface

import Cursor.Reader.Type (ReaderPlus)
import Cursor.Reader.Examples (enum, exact)
import Cursor.Reader.Utilities (true, just, repetition)
import Block (ASCII, ASCII1)
import Control.Monad.Except (ExceptT, throwError)

import qualified ASCII.Char as ASCII

newtype Error = Error Text
  deriving newtype IsString

data Request = Request Start [Field] (Maybe Body)

data Start = Start Method Target Version

data Version = Version Natural Natural

data Field = Field ASCII ASCII

data Target = Target ASCII

data Body = Body ByteString

data Method = GET | POST | HEAD deriving stock (Eq, Ord, Show, Enum, Bounded)

request :: ExceptT Error (ReaderPlus up action 'Write Word8 ByteString) Request
request = do
    s <- start
    fs <- repetition field
    blankLine
    b <- _
    pure $ Request s fs b

start :: ExceptT Error (ReaderPlus up action 'Write ASCII.Char ASCII1) Start
start = _

method :: ExceptT Error (ReaderPlus up action 'Write ASCII.Char ASCII1) Method
method = just "method" enum

space :: ExceptT Error (ReaderPlus up action 'Write ASCII.Char ASCII1) ()
space = true "space" $ exact " "
