module Step.Spec.LineHistory where

import Step.Internal.Prelude

import Hedgehog

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified ListT
import qualified ListLike
import qualified Char

import qualified Step.Document.Parser as Doc
import qualified Step.Document.Prelude as Doc

import Step.Test.InputChunking (genChunks)

import Test.Hspec
import Test.Hspec.Hedgehog

import Text (Text)

import Loc (loc)
import qualified SpanOrLoc

spec :: SpecWith ()
spec = describe "Line history" do

    describe "examples" do
        let
            lh = LineHistory
                { lineMap = Map.fromList
                    [ (Line 1, (CursorPosition 0,  Buffer.singleton "One two three\r\n"))
                    , (Line 2, (CursorPosition 15, Buffer.singleton "four five six\r"))
                    , (Line 3, (CursorPosition 29, Buffer.singleton "seven eight"Map Line (CursorPosition, Buffer text)
                , lastCharacterWasCR :: Bool
                , documentPosition :: Loc
                , cursorPosition :: CursorPosition
                }
