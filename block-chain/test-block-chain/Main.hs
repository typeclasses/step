import Essentials

import Data.Text (Text)
import Test.Hspec (describe, it, shouldBe, hspec)
import Test.Hspec.Hedgehog (hedgehog)
import Hedgehog (forAll)
import Block (Text1)

import qualified Block.Hedgehog.Gen.Shatter as Gen

main = hspec do

    describe "HTTP example" do

        it "Request line" $ hedgehog do
            let input :: Text = "GET /hello.txt HTTP/1.1"
                reader = undefined -- todo
            chunkedInput :: [Text1] <- forAll $ Gen.shatter0 input
            undefined -- todo
            -- runReader reader chunkedInput `shouldBe` ("GET", ["hello.txt"], ['1', '1'])
