import Essentials

import Block.Class.Gen (genBlocks)
import Data.Text (Text)
import Test.Hspec (describe, it, shouldBe, hspec)
import Test.Hspec.Hedgehog (hedgehog)
import Hedgehog (forAll)
import Block.Class.Text (Text1)

main = hspec do

    describe "HTTP example" do

        it "Request line" $ hedgehog do
            let input :: Text = "GET /hello.txt HTTP/1.1"
                reader = _
            chunkedInput :: [Text1] <- forAll $ genBlocks input
            _
            -- runReader reader chunkedInput `shouldBe` ("GET", ["hello.txt"], ['1', '1'])
